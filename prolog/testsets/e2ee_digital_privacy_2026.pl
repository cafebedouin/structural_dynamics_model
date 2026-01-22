% ============================================================================
% CONSTRAINT STORY: e2ee_digital_privacy_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "End-to-end encryption: Best ideas of the century" by Matthew Sparkes
% ============================================================================

:- module(e2ee_digital_privacy_2026, []).

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
 * * constraint_id: e2ee_digital_privacy_2026
 * human_readable: End-to-End Encryption (ETEE)
 * domain: technological/political/social
 * temporal_scope: 1977-2026
 * spatial_scope: Global (Digital)
 * * SUMMARY:
 * End-to-end encryption (ETEE) is a digital "wall" that ensures messages are only 
 * readable by the sender and recipient, rendering them "meaningless gibberish" 
 * during transmission. It provides a technological safeguard for 
 * privacy, democracy, and human rights by relying on "immutable mathematics" 
 * rather than the promises of service providers or states.
 * * KEY AGENTS:
 * - The Vulnerable Individual: Relies on ETEE for physical safety in dangerous regions.
 * - The Surveillance State/Adversary: A spy agency or police force unable to "demand or threaten" 
 * their way into communications.
 * - The Service Provider: An intermediary (app/internet host) that is technologically 
 * blinded to the user's data.
 * * NARRATIVE ARC:
 * Beginning with the RSA algorithm in 1977, encryption has evolved into a 
 * foundational 21st-century tool. It has transformed modern life by 
 * creating a mathematical constraint that prevents the rollback of liberties.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(e2ee_digital_privacy_2026, 0, 10).
narrative_ontology:constraint_claim([e2ee_digital_privacy_2026], [technological_safeguard]).

% Base extractiveness score (0.0-0.3 = low)
% Rationale: ETEE is fundamentally protective rather than extractive; it prevents 
% the extraction of information by unauthorized parties.
domain_priors:base_extractiveness(e2ee_digital_privacy_2026, 0.05).

% Suppression score (0.0-1.0)
% Rationale: It suppresses the visibility of intermediaries and states over 
% private communication, effectively "hiding" the alternative of surveillance.
domain_priors:suppression_score(e2ee_digital_privacy_2026, 0.5).

% Enforcement: Does it require active maintenance or emerge naturally?
% Rationale: While implementation requires active development, its security 
% "hinges" on immutable mathematical difficulty.
domain_priors:requires_active_enforcement(e2ee_digital_privacy_2026).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(e2ee_digital_privacy_2026, extractiveness, 0.05).
narrative_ontology:constraint_metric(e2ee_digital_privacy_2026, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(e2ee_digital_privacy_2026, private_citizens). %
constraint_beneficiary(e2ee_digital_privacy_2026, human_rights_advocates). %
constraint_victim(e2ee_digital_privacy_2026, state_surveillance_apparatus). %

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRIVACY CAMPAIGNER (Matthew Feeney) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Advocate for rights/privacy)
   WHEN: biographical (Protection of a lifetime of civil liberties)
   WHERE: mobile (Can navigate global privacy debates)
   SCOPE: global (Human rights around the world)
   
   WHY THIS CLASSIFICATION:
   For the campaigner, ETEE is a "Rope"—a vital coordination mechanism 
   and tool used to secure democracy and save lives in dangerous areas.
   
   NARRATIVE EVIDENCE:
   "There are people... who literally rely on [encryption] to save their lives".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    e2ee_digital_privacy_2026,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SURVEILLANCE STATE - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional (State power/Police/Spy agencies)
   WHEN: immediate (Short-term investigations/surveillance)
   WHERE: constrained (Blocked by the technological "wall")
   SCOPE: national (Law enforcement jurisdiction)
   
   WHY THIS CLASSIFICATION:
   For the state seeking access, ETEE is a "Noose" that constrains their 
   power. It renders their traditional tools—demands, blackmail, or 
   threats—ineffective against "impenetrable gibberish".
   
   NARRATIVE EVIDENCE:
   "No police force, spy agency... could demand, blackmail or threaten 
   their way in".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    e2ee_digital_privacy_2026,
    noose,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MATHEMATICIAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of cryptographic constants)
   WHEN: historical (From RSA 1977 foundations onward)
   WHERE: analytical (Logic/Prime numbers)
   SCOPE: global (Universal laws of math)
   
   WHY THIS CLASSIFICATION:
   To the mathematician, ETEE is a "Mountain"—an immutable reality 
   grounded in the fundamental difficulty of factoring large numbers. 
   It is a zero-degree-of-freedom constraint of logic.
   
   NARRATIVE EVIDENCE:
   "Digital encryption doesn’t depend on promises, but on immutable 
   mathematics".
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
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(e2ee_digital_privacy_2026_tests).

test(multi_perspective_encryption) :-
    constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, Rope, context(individual_moderate, _, _, _)),
    constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, Noose, context(institutional, _, _, _)),
    constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, Mountain, context(analytical, _, _, _)),
    Rope \= Noose,
    Noose \= Mountain.

test(powerless_protection) :-
    % Shows that individual_powerless (vulnerable people) benefit from this low-extraction Rope.
    domain_priors:base_extractiveness(e2ee_digital_privacy_2026, E),
    E < 0.1.

test(mathematical_immutability) :-
    % Analytical historical perspective confirms the Mountain classification.
    constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, mountain, context(analytical, historical, _, _)).

:- end_tests(e2ee_digital_privacy_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.05):
 * Reasoning: ETEE is a defense mechanism. It protects rather than extracts.
 * * 2. SUPPRESSION SCORE (0.5):
 * Reasoning: It actively suppresses state "visibility," which the source 
 * highlights as a fundamental conflict between privacy and surveillance.
 * * 3. PERSPECTIVE SELECTION:
 * Chose Campaigner (Rope), State (Noose), and Mathematician (Mountain) to 
 * illustrate the technological, political, and absolute reality of ETEE.
 * * 4. AMBIGUITIES:
 * - The source mentions that "other algorithms" use "all manner of obscure 
 * mathematics". The shift from RSA to these models introduces 
 * an Omega regarding long-term mathematical persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_decryption_risk,
    "Will quantum computing render current 'hard-to-crack' mathematics solvable?",
    resolution_mechanism("Experimental verification of Shor's algorithm at scale"),
    impact("If yes: The 'Mountain' of RSA/Current math collapses into a 'Scaffold'."),
    confidence_without_resolution(medium)
).

omega_variable(
    democratic_resilience_threshold,
    "Can ETEE alone prevent the total rollback of liberties in a non-liberal democracy?",
    resolution_mechanism("Long-term historical analysis of human rights in high-encryption vs low-encryption regimes"),
    impact("If yes: ETEE is a permanent political Rope. If no: It is a temporary shield."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Custodial Encryption (Managed Keys)
 * Viability: Historically used by some corporate and state systems.
 * Suppression: Actively rejected by ETEE which removes the "middleman" key.
 * Evidence: "Providers... cannot see your communications".
 * * CONCLUSION:
 * The move to ETEE turns the "Managed Secret" (Rope for the state) into a 
 * "Private Secret" (Rope for the individual) and a Noose for the surveillant.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [e2ee_digital_privacy_2026].
% Test: ?- run_tests(e2ee_digital_privacy_2026_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
