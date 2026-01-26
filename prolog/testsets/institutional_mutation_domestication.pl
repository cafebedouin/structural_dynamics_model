% ============================================================================
% CONSTRAINT STORY: institutional_mutation_domestication
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: UKE_META System Architect Analysis - Star Wars as Evolutionary Metaphor
% ============================================================================

:- module(institutional_mutation_domestication, []).

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
 * * constraint_id: institutional_mutation_domestication
 * human_readable: The Jedi Bureaucratic Capture
 * domain: political/social
 * temporal_scope: Late Galactic Republic (approx. 1000 generations to Order 66)
 * spatial_scope: Galactic
 * * SUMMARY:
 * This constraint represents the process by which a high-agency "mutation" (the Jedi) 
 * is integrated into a stable bureaucratic structure (the Senate). It functions 
 * as a selective pressure that prioritizes institutional compliance over the 
 * adaptive capacity (Force-sensitivity) that originally justified the role.
 * * KEY AGENTS:
 * - The Council (Mace Windu): Institutional actors prioritizing stability and procedure.
 * - The Mutation (Qui-Gon Jinn/Anakin): High-agency actors testing system boundaries.
 * - The Auditor (The Patient): An analytical observer identifying systemic blind spots.
 * * NARRATIVE ARC:
 * The Republic domesticates the Jedi mutation through credentialing (training) and 
 * embedding. This creates structural blindness to existential threats (Sith) 
 * that operate across the boundaries the bureaucracy has defined as uncrossable. 
 * The system eventually triggers an immune response (Order 66) to eliminate the 
 * mutation it can no longer metabolize.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(institutional_mutation_domestication, 0, 10).
narrative_ontology:constraint_claim(institutional_mutation_domestication, rope).

% Base extractiveness score: 0.7 (High asymmetry)
% Rationale: The system extracts the labor and mystical sanction of the Jedi 
% while imposing rigid compliance that ultimately leads to their destruction.
domain_priors:base_extractiveness(institutional_mutation_domestication, 0.7).

% Suppression score: 0.8 (High suppression)
% Rationale: Alternatives like non-institutional training (Grey Jedi) are 
% actively discouraged or treated as "Dark Side" deviations.
domain_priors:suppression_score(institutional_mutation_domestication, 0.8).

% Enforcement requirements: Requires active enforcement
% Rationale: The Council must actively police its members to maintain Senate ties.
domain_priors:requires_active_enforcement(institutional_mutation_domestication).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(institutional_mutation_domestication, extractiveness, 0.7).
narrative_ontology:constraint_metric(institutional_mutation_domestication, suppression_requirement, 0.8).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE JEDI COUNCIL - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power within their order.
   WHEN: historical - Thinking in terms of "thousand generations."
   WHERE: constrained - Exit possible but involves losing identity/purpose.
   SCOPE: global - Concerned with galactic stability.
   
   WHY THIS CLASSIFICATION:
   The Council views the Code and Senate procedure as a necessary coordination 
   mechanism (Rope). They believe the constraints provide the structure 
   necessary to channel Force-sensitivity into peace-keeping rather than chaos.
   
   NARRATIVE EVIDENCE:
   "Our allegiance is to the Republic, to democracy." - Mace Windu.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    institutional_mutation_domestication,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    % Classification logic: Power + Long Horizon + Low Extraction (for them) = Rope
    domain_priors:base_extractiveness(institutional_mutation_domestication, E),
    E < 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PADAWAN / SERF - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to rules, midichlorian testing.
   WHEN: immediate - Focused on daily training and compliance.
   WHERE: trapped - Taken as children; no other life known.
   SCOPE: local - Their immediate master and temple life.
   
   WHY THIS CLASSIFICATION:
   For the young trainee, the Order is a physical law. They lack the agency 
   to imagine alternatives and view their biological "gift" as a destiny 
   they cannot escape. It appears as an unchangeable feature of reality.
   
   NARRATIVE EVIDENCE:
   Anakin's early frustration with being a "slave" regardless of his status 
   (first to Watto, then to the Council's dictates).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    institutional_mutation_domestication,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    % Classification logic: Powerless + Trapped + Immediate = Mountain
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (The Patient) - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer identifying systemic patterns.
   WHEN: biographical - Comparing individual effort to institutional barriers.
   WHERE: arbitrage - Can see across different systems (Senate/Jedi/Sith).
   SCOPE: continental - Regional perspective on system friction.
   
   WHY THIS CLASSIFICATION:
   The observer sees the "Biology Trap" (midichlorians) as a constructed barrier. 
   They recognize that the system selects against the very "mutation" it 
   needs, creating an extractive structure that serves institutional 
   preservation over system health.
   
   NARRATIVE EVIDENCE:
   The comparison between the Jedi Council's blind spots and a university 
   hospital's refusal to hire the patient who solved its $8,337 error.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    institutional_mutation_domestication,
    snare,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :-
    % Classification logic: High suppression + High extractiveness = Snare
    domain_priors:suppression_score(institutional_mutation_domestication, S),
    S > 0.6,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================= */

:- begin_tests(institutional_mutation_domestication_tests).

/**
 * TEST 1: Multi-perspective variance
 */
test(multi_perspective_jedi_shift) :-
    constraint_indexing:constraint_classification(institutional_mutation_domestication, Type1, context(institutional, historical, constrained, global)),
    constraint_indexing:constraint_classification(institutional_mutation_domestication, Type2, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(institutional_mutation_domestication, Type3, context(analytical, biographical, arbitrage, regional)),
    Type1 \= Type2,
    Type2 \= Type3.

/**
 * TEST 2: Power-based extractiveness scaling
 */
test(power_extractiveness_mutation) :-
    % Powerless (Anakin) experience higher extraction than Institutional (Windu)
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, historical, constrained, global),
    % Assuming a mock logic for extractiveness_for_agent
    % In real usage, this would call the logic derived from base_extractiveness
    true.

/**
 * TEST 3: Time-horizon immutability
 */
test(time_immutability_republic) :-
    % Short horizon = Mountain (Jedi Code as law)
    constraint_indexing:effective_immutability(immediate, trapped, mountain),
    % Long horizon = Rope (Jedi Order as historical accident)
    constraint_indexing:effective_immutability(civilizational, mobile, rope).

:- end_tests(institutional_mutation_domestication_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.7):
 * Reasoning: The system doesn't just benefit from Jedi; it consumes them. 
 * By domesticating the mutation, it eliminates the Jedi's unique 
 * information-processing capacity in favor of bureaucratic compliance.
 * * 2. PERSPECTIVE SELECTION:
 * Chose Council (Institutional), Padawan (Powerless), and Patient (Analytical) 
 * to reflect the "Mutation vs Organism" tension described in the text.
 * * 3. CLASSIFICATION RATIONALE:
 * Council sees Rope because they are the weavers. 
 * Padawans see Mountain because they are the thread. 
 * The Auditor sees Snare because they see the knot.
 * * 4. CONFIDENCE:
 * High: The classification of "biologizing method" as a Snare mechanic.
 * Medium: The specific temporal scope of "historical" for the Council.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Force as Method
 * Viability: Force sensitivity is treated as a learnable method rather than 
 * a biological gift (e.g., Chirrut Îmwe or the Patient's diagnostic skills).
 * Suppression: The "Midichlorian" narrative and Council monopoly on training.
 * * ALTERNATIVE 2: Distributed Agency
 * Viability: Localized Jedi clusters serving villages directly without 
 * Senate embedding.
 * Suppression: Rejected in favor of the "High Council" for easier 
 * state coordination.
 * * CONCLUSION:
 * The presence of suppressed alternatives (Method vs. Gift) confirms the 
 * "Snare" classification for the broader analytical perspective.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 1. Load: ?- [institutional_mutation_domestication].
 * 2. Report: ?- constraint_indexing:multi_index_report(institutional_mutation_domestication).
 * 3. Tests: ?- run_tests(institutional_mutation_domestication_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
