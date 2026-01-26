% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: starwars_evolutionary_mutation
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: starwars_evolution.md (Synthesis of UKE_D v1.4 protocol)
% ============================================================================

:- module(constraint_starwars_evolution, []).

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
 * * constraint_id: starwars_evolutionary_mutation
 * human_readable: Jedi as Systemic Evolutionary Outliers
 * domain: social/political/biological
 * temporal_scope: "A long time ago" / Galactic Republic Era
 * spatial_scope: Galactic (Coruscant/Jedi Temple)
 * * SUMMARY:
 * This constraint models the Jedi not as moral paragons, but as systemic 
 * outliers—mutations that test which institutional "necessities" are genuine 
 * physical constraints and which are merely bureaucratic artifacts. 
 * The Jedi Council's embedding within the Senate bureaucracy created a 
 * structural blindness, where the "Domestication Gradient" turned a beneficial 
 * mutation into a rigid apparatus.
 * * KEY AGENTS:
 * - Individual Jedi (Outlier): Individual powerless; possesses latent diagnostic 
 * capacity but is excluded or filtered by the institution if they challenge 
 * boundary conditions.
 * - Jedi Council (Domesticated): Institutional; rule-making power blinded by 
 * its own bureaucratic embedding.
 * - The Sith (Selection Pressure): Analytical/Powerful; external force that 
 * applies the "fitness test" the Council failed to perform internally.
 * * NARRATIVE ARC:
 * What began as a Rope (Jedi as functional coordination for peace) ossified 
 * into a Mountain (the unchangeable "Will of the Force" as interpreted by 
 * bureaucracy). Ultimately, this domestication became a Snare, as the 
 * Council's inability to metabolize dissenting "mutations" (like Anakin) 
 * extracted the entire order's survival in favor of institutional conformity 
 *.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the Deferential Realism suite
narrative_ontology:interval(galactic_selection_event, 0, 10).
narrative_ontology:constraint_claim(starwars_evolutionary_mutation, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.75. The institution "extracted" the individual diagnostic 
% potential of its members to feed its bureaucratic survival until total 
% collapse.
domain_priors:base_extractiveness(starwars_evolutionary_mutation, 0.75).

% Suppression score (0.0-1.0)
% Rationale: 0.8. The Council actively suppressed boundary-testing and 
% alternative diagnostic methods in favor of "conformity" filters.
domain_priors:suppression_score(starwars_evolutionary_mutation, 0.8).

% Enforcement requirements
% Requires active enforcement (The Jedi Code, Senate oversight, and the 
% rigid master-apprentice training recursive loop).
domain_priors:requires_active_enforcement(starwars_evolutionary_mutation).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(starwars_evolutionary_mutation, extractiveness, 0.75).
narrative_ontology:constraint_metric(starwars_evolutionary_mutation, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(starwars_evolutionary_mutation, bureaucratic_inertia).
constraint_beneficiary(starwars_evolutionary_mutation, the_sith_plan).
constraint_victim(starwars_evolutionary_mutation, individual_diagnostic_ability).
constraint_victim(starwars_evolutionary_mutation, the_jedi_lineage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ROGUE JEDI (QUI-GON/ANAKIN) - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Despite physical power, they lack the agency 
         to change the Council's structural blindness.
   WHEN: immediate - Tactical focus on perceived threats the Council ignores.
   WHERE: trapped - Bound by the "conformity filters" of the Order.
   SCOPE: local - Individual missions and intuitive detections.
   
   WHY THIS CLASSIFICATION:
   To a Jedi who sees a threat (like the return of the Sith), the Council's 
   denial is a Mountain. It is an unchangeable feature of the institutional 
   landscape that no amount of evidence can shift because the Council has 
   delegated its "diagnostic sensors" to the Senate.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    starwars_evolutionary_mutation,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(starwars_evolutionary_mutation, S),
    S > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE JEDI COUNCIL - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power governing the Order.
   WHEN: generational - Maintaining the "thousand generations" of peace.
   WHERE: mobile - Believe they are coordinating the Republic's security.
   SCOPE: national - Galactic-scale management.
   
   WHY THIS CLASSIFICATION:
   The Council views its rules and Senate relationship as a Rope—a functional 
   coordination mechanism that ensures the Jedi serve the "greater good" 
   of the Republic. They view deviations as 
   failures of discipline rather than vital signals.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    starwars_evolutionary_mutation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(starwars_evolutionary_mutation, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PALPATINE / THE SITH - Snare
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the Order's decline.
   WHEN: historical - Executing a plan centuries in the making.
   WHERE: analytical - Not bound by the Jedi Code.
   SCOPE: global - Total galactic control.
   
   WHY THIS CLASSIFICATION:
   From the Sith perspective, the Council's own "domestication" is a Snare. 
   By tethering the Jedi to a decaying political system, they have turned 
   the Order's own "Rope" of coordination into the mechanism of its 
   execution.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    starwars_evolutionary_mutation,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(starwars_evolutionary_mutation, S),
    S > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE JEDI COUNCIL / GALACTIC ORDER (INSTITUTIONAL) - Rope
   --------------------------------------------------------------------------
   WHO: agent_power(institutional) - The governing bodies of the Force.
   WHEN: generational - Managing the legacy of the Order across millennia.
   WHERE: arbitrage - Managing the tension between Dark and Light.
   SCOPE: global - Affecting the spiritual and political balance of the galaxy.
   
   WHY THIS CLASSIFICATION:
   The Order's Code is a Rope. It is a high-fidelity coordination mechanism 
   designed to "pull" Force-sensitive mutations toward a stable, selfless 
   equilibrium. While it provides the slack for individual growth, it 
   tightens against attachment and ego to prevent the system from 
   spiraling into Sith-led margin collapse.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    starwars_evolutionary_mutation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:requires_active_enforcement(starwars_evolutionary_mutation),
    !.

% Explicit priors reflecting the coordination energy of institutional codes.
domain_priors:base_extractiveness(starwars_evolutionary_mutation, 0.2).
domain_priors:suppression_score(starwars_evolutionary_mutation, 0.6).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(starwars_evolution_tests).

test(multi_perspective_gap) :-
    % Outlier (Mountain) vs Council (Rope) vs Sith (Snare)
    constraint_indexing:constraint_classification(starwars_evolutionary_mutation, T1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(starwars_evolutionary_mutation, T2, context(institutional, generational, mobile, national)),
    constraint_indexing:constraint_classification(starwars_evolutionary_mutation, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(extraction_conformity_link) :-
    % Conformity selection (suppression) correlates with high extraction of potential
    domain_priors:suppression_score(starwars_evolutionary_mutation, S),
    domain_priors:base_extractiveness(starwars_evolutionary_mutation, E),
    S > 0.7, E > 0.6.

:- end_tests(starwars_evolution_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. THE JEDI AS MUTATIONS: The primary insight is that "Force sensitivity" 
 * is a metaphor for high-fidelity systemic diagnostic ability. 
 * 2. THE DOMESTICATION GRADIENT: The Snare classification reflects the 
 * "trap" of institutional success—the Order became so efficient at 
 * coordinating (Rope) that it eliminated the "friction" of internal 
 * dissent, which is where its actual diagnostic power resided.
 * 3. ANALYTICAL VIEW: The Sith function as the external "Fitness Test" 
 * that the Order could no longer perform itself.
 */

omega_variable(
    training_conformity_ratio,
    "Does Jedi training select for obedience (Snare) or boundary-testing (Scaffold)?",
    resolution_mechanism("Analysis of Padawan failure rates vs. their success in identifying external systemic threats"),
    impact("If Obedience: The Order is a terminal Snare. If Boundary-Testing: It 
            is an adaptive Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Non-Bureaucratic Jedi Order
 * Viability: The "Wayseeker" or itinerant knight model allows for local 
 * systemic diagnosis without institutional blindness.
 * Suppression: Actively discouraged by the Council to maintain "centralized 
 * coordination" (The Rope).
 * * CONCLUSION:
 * The existence of viable, less-domesticated alternatives that are 
 * institutionally suppressed confirms the "Snare" status of the Council era Order.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [starwars_evolutionary_mutation].
% 2. Analyze: ?- multi_index_report(starwars_evolutionary_mutation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
