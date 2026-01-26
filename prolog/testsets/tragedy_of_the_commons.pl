% ============================================================================
% CONSTRAINT STORY: tragedy_of_the_commons
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Economic Theory / Garrett Hardin (1968)
% ============================================================================

:- module(constraint_tragedy_of_the_commons, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: tragedy_of_the_commons
 * human_readable: The Tragedy of the Commons
 * domain: economic/social
 * temporal_scope: Historical to Future
 * spatial_scope: Local to Global
 * * SUMMARY:
 * A situation where individual users, acting independently according to their 
 * own self-interest, behave contrary to the common good of all users by 
 * depleting or spoiling a shared resource through their collective action. 
 * The constraint is the mathematical inevitability of ruin in an unregulated 
 * commons with rational actors.
 * * KEY AGENTS:
 * - The Individual Herder: Seeks to maximize private gain by adding livestock.
 * - The Regulatory Body: Seeks to enforce limits to preserve the resource.
 * - The Future Generation: The silent victim of current resource depletion.
 * * NARRATIVE ARC:
 * The constraint functions as a "logic of ruin." Without a change in the 
 * structure of the game (privatization or "mutual coercion, mutually agreed 
 * upon"), the system trends toward total collapse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(tragedy_of_the_commons, 0, 10).
narrative_ontology:constraint_claim(tragedy_of_the_commons, mountain).

% Base extractiveness: 0.7 (High)
% Rationale: The individual extracts 100% of the benefit of an extra unit of 
% use, while the cost (depletion) is shared among all participants.
domain_priors:base_extractiveness(tragedy_of_the_commons, 0.7).

% Suppression score: 0.4 (Moderate)
% Rationale: While alternatives (Ostrom-style collective management) exist, 
% the "logic of ruin" often suppresses cooperation by rewarding defectors 
% in the short term.
domain_priors:suppression_score(tragedy_of_the_commons, 0.4).

% Enforcement requirements: Emerges naturally from uncoordinated rational self-interest.
domain_priors:emerges_naturally(tragedy_of_the_commons).

% Beneficiaries: The "First Mover" or the aggressive defector.
constraint_beneficiary(tragedy_of_the_commons, aggressive_defector).

% Victims: The collective community and future users.
constraint_victim(tragedy_of_the_commons, collective_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ANALYTIC OBSERVER - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: Analytical/Scientific power (Game Theorist).
   WHEN: Historical/Generational (Viewing the long-term trend).
   WHERE: Trapped (Mathematics of the payoff matrix is fixed).
   SCOPE: Global.
   
   WHY THIS CLASSIFICATION:
   From a game-theoretic perspective, the tragedy is a Mountain. The Nash 
   Equilibrium is a structural feature of the payoff matrix; it is a 
   mathematical law that rational actors will over-consume unless the 
   game itself is fundamentally altered.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tragedy_of_the_commons,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COMMUNITY MEMBER (Ostrom-style) - ROPE
   --------------------------------------------------------------------------
   WHO: Collective Organized (A group with high social capital).
   WHEN: Biographical (Lifetime of the resource).
   WHERE: Mobile/Arbitrage (Can create local rules/sanctions).
   SCOPE: Local.
   
   WHY THIS CLASSIFICATION:
   In a high-trust local community, the tragedy is a Rope. It is a coordination 
   challenge that is solved through social norms, mutual monitoring, and 
   graduated sanctions. It is a tool for sustainable living rather than 
   an inevitable doom.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tragedy_of_the_commons,
    rope,
    context(
        agent_power(collective_organized),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SUBSISTENCE USER - NOOSE
   --------------------------------------------------------------------------
   WHO: Individual Powerless (Desperate actor).
   WHEN: Immediate (Need to eat today).
   WHERE: Trapped (No alternative resource).
   SCOPE: Regional.
   
   WHY THIS CLASSIFICATION:
   For a person with no other options, the tragedy is a Snare. They are forced 
   by the structure of the system to participate in the destruction of the 
   very resource they rely on. Every extra animal they add slightly hastens 
   their eventual starvation, but failing to add it guarantees starvation today.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tragedy_of_the_commons,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE REGULATORY BODY - ROPE
   --------------------------------------------------------------------------
   WHO: Institutional (e.g., a government agency).
   WHEN: Generational (Long-term management of the resource).
   WHERE: Arbitrage (Can set quotas, fines, and other rules).
   SCOPE: Regional to National.
   
   WHY THIS CLASSIFICATION:
   For a regulatory body, the tragedy is a coordination problem to be solved 
   with the "Rope" of "mutual coercion, mutually agreed upon." They create 
   and enforce rules (like quotas or taxes) to align individual incentives 
   with the long-term health of the commons.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    tragedy_of_the_commons,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(tragedy_of_the_commons_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that the tragedy of the commons is viewed differently across agents.
 */
test(multi_perspective_variance) :-
    % Analytic Observer (Mountain)
    constraint_indexing:constraint_classification(
        tragedy_of_the_commons,
        Type1,
        context(agent_power(analytical), time_horizon(generational), exit_options(trapped), spatial_scope(global))
    ),
    % Community Member (Rope)
    constraint_indexing:constraint_classification(
        tragedy_of_the_commons,
        Type2,
        context(agent_power(collective_organized), time_horizon(biographical), exit_options(mobile), spatial_scope(local))
    ),
    % Subsistence User (Snare)
    constraint_indexing:constraint_classification(
        tragedy_of_the_commons,
        Type3,
        context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(regional))
    ),
    % Regulatory Body (Rope)
    constraint_indexing:constraint_classification(
        tragedy_of_the_commons,
        Type4,
        context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(national))
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3. % Ensure all three are distinct

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that powerless users experience higher extraction than those who can organize or regulate.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(regional)),
    ContextPowerful = context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(national)),
    constraint_indexing:extractiveness_for_agent(tragedy_of_the_commons, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(tragedy_of_the_commons, ContextPowerful, Score2),
    Score1 > Score2.  % Powerless experience more extraction

/**
 * TEST 3: Time-horizon immutability
 * Demonstrates that short-term desperation makes the tragedy a 'Snare', while long-term planning sees it as a manageable 'Rope'.
 */
test(time_immutability) :-
    % Short-term (immediate) sees it as a Snare
    constraint_indexing:effective_immutability(time_horizon(immediate), exit_options(trapped), snare),
    % Long-term (biographical/generational) sees it as a Rope
    constraint_indexing:effective_immutability(time_horizon(biographical), exit_options(mobile), rope).

/**
 * TEST 4: Domain-specific insight - Ostrom's Law vs Hardin's Law
 * Demonstrates the conflict between top-down regulation (Hardin) and bottom-up collective action (Ostrom).
 */
test(ostrom_vs_hardin) :-
    constraint_indexing:constraint_classification(tragedy_of_the_commons, ClassificationOstrom, context(agent_power(collective_organized), _, _, _)),
    constraint_indexing:constraint_classification(tragedy_of_the_commons, ClassificationHardin, context(agent_power(institutional), _, _, _)),
    ClassificationOstrom = rope,
    ClassificationHardin = rope. % Both are seen as a rope, but with different approaches.

:- end_tests(tragedy_of_the_commons_tests).

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
 * 1. EXTRACTIVENESS SCORE (0.7):
 *    The "extraction" here is the privatization of profit and the socialization 
 *    of cost. It is a highly asymmetric mechanism for value transfer from 
 *    the group to the individual.
 * 
 * 2. PERSPECTIVE SELECTION:
 *    Included the 'Collective Organized' perspective to acknowledge Elinor 
 *    Ostrom's work, which demonstrates that the tragedy is NOT a Mountain 
 *    at the local scale if social capital exists. Added 'Regulatory Body' to
 *    represent Hardin's proposed solution.
 * 
 * 3. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        human_cooperation_limit,
 *        "Can human cooperative norms scale to global spatial_scope without institutional hierarchy?",
 *        resolution_mechanism("Observation of global climate agreements and their enforcement rates"),
 *        impact("If Yes: Global commons are a Rope. If No: Global commons are a Mountain (Hardin's view)."),
 *        confidence_without_resolution(medium)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Private Property Rights
 *    Viability: High (The standard capitalist solution).
 *    Suppression: Low.
 * 
 * ALTERNATIVE 2: Ostrom's Common Pool Resource (CPR) Management
 *    Viability: High (Empirically proven in small/medium groups).
 *    Suppression: Moderate (Often ignored by centralized state bureaucracies 
 *    who prefer top-down control or total privatization).
 * 
 * CONCLUSION:
 * The existence of Alternative 2 (CPR Management) means that the "Tragedy" 
 * is only a Mountain if one assumes agents are isolated/incapable of 
 * communication. If communication is possible, it shifts toward a Rope.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/tragedy_of_the_commons].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(tragedy_of_the_commons).
 * 
 * 3. Run tests:
 *    ?- run_tests(tragedy_of_the_commons_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(tragedy_of_the_commons).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(tragedy_of_the_commons, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
