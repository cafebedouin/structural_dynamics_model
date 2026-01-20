% ============================================================================
% CONSTRAINT STORY: cinderella_midnight_deadline
% ============================================================================
% Generated: 2024-05-22
% Model: Gemini 2.0 Flash
% Source: Project Gutenberg - Cinderella and The Three Bears
% ============================================================================

:- module(constraint_cinderella_midnight, []).

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
 * * constraint_id: midnight_deadline
 * human_readable: The Fairy Godmother's Midnight Command
 * domain: magical/social
 * temporal_scope: Mythic/Folk Period
 * spatial_scope: The Kingdom (Palace and Household)
 * * SUMMARY:
 * The fairy godmother grants Cinderella a high-status transformation (magic coach, horses, 
 * gown) under a rigid temporal constraint: everything reverts to its original, low-status 
 * state (pumpkin, mice, rags) at exactly midnight. This deadline defines the limits of 
 * Cinderella's social mobility.
 * * KEY AGENTS:
 * - Cinderella: Individual powerless; subject to the magic's binary state.
 * - Fairy Godmother: Institutional/Analytical; the source and enforcer of the rule.
 * - The Prince: Individual moderate; searching for a specific fit within the constraint's aftermath.
 * * NARRATIVE ARC:
 * The constraint initially acts as a "Rope" (enabling the ball attendance), but as midnight 
 * approaches, it shifts into a "Mountain" (the inevitable physical reversion). The 
 * aftermath (the slipper) acts as a secondary "Rope" for the Prince to find her.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(midnight_deadline_period, 0, 10).
narrative_ontology:constraint_claim(midnight_deadline, mountain).

% Base extractiveness score: 0.1
% Rationale: The magic is largely a gift; it does not extract value, 
% but it is highly asymmetric in its withdrawal.
domain_priors:base_extractiveness(midnight_deadline, 0.1).

% Suppression score: 0.9
% Rationale: Magic overrides all alternatives (stalling, negotiation, or disguise). 
% Reversion is absolute.
domain_priors:suppression_score(midnight_deadline, 0.9).

% Enforcement requirements: Emerges naturally from magical law.
domain_priors:emerges_naturally(midnight_deadline).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(midnight_deadline, extractiveness, 0.1).
narrative_ontology:constraint_metric(midnight_deadline, suppression_requirement, 0.9).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(midnight_deadline, prince_search).
constraint_victim(midnight_deadline, cinderella_status).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: CINDERELLA - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For Cinderella, the stroke of twelve is as immutable as a law of physics. 
   She cannot "negotiate" with the clock. It is a Mountain that defines the 
   boundary between her dream and her reality.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    midnight_deadline,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(midnight_deadline, []),
        constraint_victim(midnight_deadline, cinderella_status),
        spatial_scope(regional)
    )
) :-
    domain_priors:suppression_score(midnight_deadline, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PRINCE / HERALD - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: biographical
   WHERE: arbitrage
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   The slipper left behind becomes a "Rope"—a functional coordination mechanism. 
   The Prince uses it to filter the population and find the specific agent 
   that fits the "test." It is beneficial and used for social sorting.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    midnight_deadline,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        constraint_beneficiary(midnight_deadline, prince_search),
        constraint_victim(midnight_deadline, []),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE STEPSISTERS - Noose
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: immediate
   WHERE: constrained
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   The slipper test acts as a "Noose" for the sisters. It is a coercive 
   requirement (marrying the Prince) that they attempt to satisfy but 
   physically cannot. It extracts their pride and reveals their inferiority.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    midnight_deadline,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(midnight_deadline, prince_search),
        constraint_victim(midnight_deadline, stepsisters),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(midnight_deadline, E),
    E < 0.2,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cinderella_tests).

test(perspectival_gap) :-
    % Cinderella sees a Mountain; the Prince sees a Rope.
    constraint_indexing:constraint_classification(midnight_deadline, mountain, context(agent_power(individual_powerless), _, _, _, _, _)),
    constraint_indexing:constraint_classification(midnight_deadline, rope, context(agent_power(institutional), _, _, _, _, _)).

test(extraction_limit) :-
    % The sisters' failure is the "Noose" experience.
    constraint_indexing:constraint_classification(midnight_deadline, noose, context(agent_power(individual_moderate), _, _, _, stepsisters, _)).

:- end_tests(cinderella_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * * The primary insight here is the duality of the deadline. Magic acts as a 
 * Mountain (Law) for the subject, but the Prince uses the fallout (the shoe) 
 * as a Rope (Tool).
 * * The "Goldilocks" text (also provided) mirrors this in the "Size/Property" 
 * constraint. The beds and porridge bowls act as physical Mountains for 
 * Goldilocks (Too hard/hot = zero utility), while they are Ropes for the bears 
 * (providing comfort and nutrition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporal_sync,
    "Was the Palace clock synced with the Fairy Godmother's internal clock?",
    resolution_mechanism("Verification of the precise moment of transformation vs. the Prince's observation."),
    impact("If out of sync, the 'Mountain' becomes a 'Noose' of embarrassment."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Honest Courting
 * Viability: High. Cinderella could have attended without magic in her best rags.
 * Suppression: Actively suppressed by the stepmother's social exclusion and 
 * chores (the original household Noose).
 * Evidence: "It would only make the people laugh to see a Cinderwench at a ball."
 * * CONCLUSION:
 * The magic is only necessary because the social hierarchy of the household 
 * is a Noose that suppresses the "Rope" of normal social interaction.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(cinderella_midnight_deadline, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(cinderella_midnight_deadline, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(cinderella_midnight_deadline, noose, agent_power(individual_powerless)).
