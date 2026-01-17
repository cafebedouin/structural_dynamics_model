% ============================================================================
% CONSTRAINT STORY: biological_curiosity
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Berlyne (1960) / Schmidhuber (1991) / Information Foraging Theory
% ============================================================================

:- module(constraint_curiosity, []).

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
 * * constraint_id: biological_curiosity
 * human_readable: Curiosity (The Information-Seeking Drive)
 * domain: biological/technological/social
 * temporal_scope: Biographical to Civilizational
 * spatial_scope: Global
 * * SUMMARY:
 * Curiosity is the innate biological drive to seek out new information and reduce 
 * uncertainty in the environment. It acts as the "Intrinsic Motivation" that 
 * offsets the high costs of exploration, ensuring an agent does not settle 
 * for a suboptimal local peak.
 * * KEY AGENTS:
 * - The Explorer: The agent driven by novelty-seeking (high curiosity).
 * - The Routine-Seeker: The agent driven by safety and exploitation (low curiosity).
 * - The Environment: The landscape of varying entropy and potential rewards.
 * * NARRATIVE ARC:
 * Curiosity functions as a "Mental Rope." It pulls the agent away from the 
 * "Safety Noose" of the known. While it carries a "Distraction Tax," it is 
 * the primary mechanism for upgrading an agent's internal map to find 
 * Global Optima that are invisible to pure exploiters.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(biological_curiosity, 0, 10).
narrative_ontology:constraint_claim(biological_curiosity, intrinsic_motivation).

% Base extractiveness score (0.15)
% Low extraction; curiosity "extracts" energy for search, but usually 
% returns higher value in long-term survivability.
domain_priors:base_extractiveness(biological_curiosity, 0.15).

% Suppression score (0.2)
% Low suppression; curiosity is the enemy of suppression, as it 
% actively seeks to uncover hidden alternatives.
domain_priors:suppression_score(biological_curiosity, 0.2).

% Enforcement requirements
domain_priors:emerges_naturally(biological_curiosity).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(biological_curiosity, extractiveness, 0.15).
narrative_ontology:constraint_metric(biological_curiosity, suppression_requirement, 0.2).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(biological_curiosity, the_scientific_method).
constraint_beneficiary(biological_curiosity, nomadic_cultures).
constraint_victim(biological_curiosity, institutional_predictability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DISCOVERER - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (has enough 'slack' to afford the search)
   WHEN: biographical (long-term growth)
   WHERE: mobile (high exploration capability)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the agent with surplus energy, curiosity is a "Rope." It coordinates 
   their internal resources to engage with the unknown. It allows them 
   to climb out of stagnant environments and discover new "Mountains" 
   of opportunity.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    biological_curiosity,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(biological_curiosity, E),
    E < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRAPPED SUBJECT - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (highly constrained or punished for deviance)
   WHEN: immediate
   WHERE: trapped (in a rigid or high-danger environment)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   In a highly restrictive environment (like a prison or a rigid 
   bureaucracy), curiosity is a "Noose." Seeking out information 
   leads to punishment or psychological despair, as the agent 
   discovers alternatives they are legally or physically unable to reach.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    biological_curiosity,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(biological_curiosity, S),
    S > 0.1,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE NEUROSCIENTIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The observer sees the "Dopaminergic System" as a "Mountain." 
   The reward for information-seeking is a hard-coded feature of the 
   mammalian brain. It is an immutable law of behavioral biology 
   designed to solve the Exploration/Exploitation trade-off.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    biological_curiosity,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(biological_curiosity),
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(curiosity_tests).

test(curiosity_as_exit_strategy) :-
    % Testing that curiosity acts as a Rope for mobile agents seeking the 'Global Optimum'.
    constraint_indexing:constraint_classification(biological_curiosity, rope, context(_, _, mobile, _)).

test(curiosity_punishment_noose) :-
    % Testing that in 'trapped' contexts, curiosity becomes a psychological Noose.
    constraint_indexing:constraint_classification(biological_curiosity, noose, context(_, _, trapped, _)).

:- end_tests(curiosity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * Curiosity is the biological implementation of the 'Exploration' 
 * directive. I set extractiveness low because it is an investment 
 * in the self. I chose "Noose" for the trapped perspective to 
 * reflect that curiosity without agency creates suffering (The Pandora Effect).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    information_overload_threshold,
    "At what point does the search for information (Curiosity) stop being a Rope and start being a 'Noose of Distraction'?",
    resolution_mechanism("Long-term studies on focus-depletion in high-entropy digital environments"),
    impact("If Overload is High: The Rope of Curiosity snaps; agents fall into the Noose of paralysis."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
