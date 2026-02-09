% ============================================================================
% CONSTRAINT STORY: biological_curiosity
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Berlyne (1960) / Schmidhuber (1991) / Information Foraging Theory
% ============================================================================

:- module(constraint_biological_curiosity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
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
 * "Safety Snare" of the known. While it carries a "Distraction Tax," it is 
 * the primary mechanism for upgrading an agent's internal map to find 
 * Global Optima that are invisible to pure exploiters.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(biological_curiosity, 0, 10).
narrative_ontology:constraint_claim(biological_curiosity, snare).

% Base extractiveness score (0.15)
% Low extraction; curiosity "extracts" energy for search, but usually 
% returns higher value in long-term survivability.
domain_priors:base_extractiveness(biological_curiosity, 0.15).

% Suppression score (0.2)
% Low suppression; curiosity is the enemy of suppression, as it 
% actively seeks to uncover hidden alternatives.
domain_priors:suppression_score(biological_curiosity, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(biological_curiosity, extractiveness, 0.15).
narrative_ontology:constraint_metric(biological_curiosity, suppression_requirement, 0.2).

% Enforcement requirements
domain_priors:emerges_naturally(biological_curiosity).



% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(biological_curiosity, the_scientific_method).
narrative_ontology:constraint_beneficiary(biological_curiosity, nomadic_cultures).
narrative_ontology:constraint_victim(biological_curiosity, institutional_predictability).

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
    E < 0.3.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRAPPED SUBJECT - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (highly constrained or punished for deviance)
   WHEN: immediate
   WHERE: trapped (in a rigid or high-danger environment)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   In a highly restrictive environment (like a prison or a rigid 
   bureaucracy), curiosity is a "Snare." Seeking out information 
   leads to punishment or psychological despair, as the agent 
   discovers alternatives they are legally or physically unable to reach.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    biological_curiosity,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(biological_curiosity, S),

    S > 0.1.

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
    domain_priors:emerges_naturally(biological_curiosity).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE EDUCATIONAL SYSTEM - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Designs and implements learning environments)
   WHEN: biographical (The duration of a student's education)
   WHERE: arbitrage (Can adapt curriculum and pedagogy to foster curiosity)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For an educational system, curiosity is a "Rope." It is a fundamental 
   biological drive that, when properly channeled, can be used as a powerful 
   coordination mechanism for learning, exploration, and the transmission of 
   knowledge. The system designs its "curriculum" to pull students towards 
   new information.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    biological_curiosity,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(biological_curiosity_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that curiosity is experienced differently across agents.
 */
test(multi_perspective_variance) :-
    % Discoverer (Rope)
    constraint_indexing:constraint_classification(
        biological_curiosity,
        Type1,
        context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(global))
    ),
    % Trapped Subject (Snare)
    constraint_indexing:constraint_classification(
        biological_curiosity,
        Type2,
        context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))
    ),
    % Neuroscientist (Mountain)
    constraint_indexing:constraint_classification(
        biological_curiosity,
        Type3,
        context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))
    ),
    % Educational System (Rope)
    constraint_indexing:constraint_classification(
        biological_curiosity,
        Type4,
        context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national))
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3. % Rope, Snare, Mountain are different

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that powerless agents (trapped subject) experience higher extraction (frustration/despair)
 * compared to powerful agents (educational system) who utilize it as a tool.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local)),
    ContextPowerful = context(agent_power(institutional), time_horizon(biographical), exit_options(arbitrage), spatial_scope(national)),
    constraint_indexing:extractiveness_for_agent(biological_curiosity, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(biological_curiosity, ContextPowerful, Score2),
    Score1 > Score2.  % Powerless experience more extraction

/**
 * TEST 3: Time-horizon immutability
 * Demonstrates that while individual acts of curiosity are flexible, the biological drive itself is immutable.
 */
test(time_immutability) :-
    % Short horizon (immediate) sees individual acts of curiosity (Rope)
    constraint_indexing:effective_immutability(time_horizon(immediate), exit_options(mobile), rope),
    % Long horizon (civilizational) sees the biological drive as an immutable fact (Mountain).
    constraint_indexing:effective_immutability(time_horizon(civilizational), exit_options(analytical), mountain).

/**
 * TEST 4: Domain-specific insight - The Pandora Effect
 * Demonstrates that curiosity without agency can lead to despair.
 */
test(pandora_effect) :-
    constraint_indexing:constraint_classification(biological_curiosity, ClassificationTrapped, context(agent_power(powerless), _, exit_options(trapped), _)),
    ClassificationTrapped = snare.

:- end_tests(biological_curiosity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini 2.0 Flash
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. BASE EXTRACTIVENESS (0.15): Chose low because curiosity is an investment 
 * in the self, leading to long-term gains in knowledge and adaptability. 
 * The "extraction" is primarily of energy and attention.
 * 
 * 2. SUPPRESSION (0.2): Low suppression because curiosity by its nature seeks to 
 * uncover hidden alternatives and resists attempts to limit information.
 * 
 * 3. PERSPECTIVE SELECTION:
 *    Selected Explorer (Rope), Trapped Subject (Snare), Neuroscientist (Mountain), and Educational System (Rope) 
 *    to illustrate the dynamic nature of curiosity across different contexts and agent powers.
 * 
 * 4. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        information_overload_threshold,
 *        "At what point does the search for information (Curiosity) stop being a Rope and start being a 'Snare of Distraction'?",
 *        resolution_mechanism("Long-term studies on focus-depletion in high-entropy digital environments"),
 *        impact("If Overload is High: The Rope of Curiosity snaps; agents fall into the Snare of paralysis."),
 *        confidence_without_resolution(medium)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES: Exploitation without Exploration
 * 
 * ALTERNATIVE 1: Pure Exploitation/Routine Seeking
 *    Viability: In highly stable and predictable environments, focusing solely on exploiting known resources can be highly efficient in the short term.
 *    Suppression: Curiosity acts against this by continuously seeking novelty. However, systems can suppress curiosity to enforce routine.
 *    Evidence: Highly specialized factory workers, rigid bureaucratic systems.
 * 
 * CONCLUSION:
 * While pure exploitation is a viable short-term strategy in stable environments, the biological imperative of curiosity is suppressed at a cost to adaptability and long-term survival. Systems that actively suppress curiosity risk becoming fragile in the face of environmental change.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/biological_curiosity].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(biological_curiosity).
 * 
 * 3. Run tests:
 *    ?- run_tests(biological_curiosity_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(biological_curiosity).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(biological_curiosity, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
