% ============================================================================
% CONSTRAINT STORY: nash_equilibrium_coordination
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: John Nash (1950) / Non-Cooperative Games
% ============================================================================

:- module(constraint_nash_equilibrium, []).

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
 * * constraint_id: nash_equilibrium_coordination
 * human_readable: Nash Equilibrium
 * domain: economic/social/political
 * temporal_scope: 1950 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Strategic Interaction)
 * * SUMMARY:
 * A Nash Equilibrium is a state in a non-cooperative game where no player can 
 * improve their outcome by changing their strategy unilaterally, given the 
 * strategies of others. It represents a fundamental structural "gravity" that 
 * stabilizes social and economic systems, often at sub-optimal levels.
 * * KEY AGENTS:
 * - The Strategic Actor (Subject): The powerless agent trapped in a state 
 * where "doing the right thing" alone results in a worse personal outcome.
 * - The Institutional Architect: An agent who uses the equilibrium as a 
 * "Rope" to design market rules or voting systems that stabilize desired goals.
 * - The Monopoly/Cartel: An institutional agent that treats the equilibrium 
 * as a "Noose" to extract value from consumers by ensuring no firm has an 
 * incentive to lower prices.
 * * NARRATIVE ARC:
 * The Nash Equilibrium is the "Mountain" of social interaction—an inescapable 
 * logic of stability. In healthy markets, it is a "Rope" for functional 
 * coordination (e.g., standardizing traffic laws). However, in systemic 
 * failures like poverty traps or price wars, the equilibrium acts as a 
 * "Noose," extracting the potential for progress (extraction) and strangling 
 * the possibility of collective escape.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(nash_era, 1950, 2026).
narrative_ontology:constraint_claim(nash_equilibrium_coordination, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.5. The equilibrium is moderately extractive; it often "takes" 
% the potential for a better collective outcome (Pareto improvement) to 
% maintain individual stability.
domain_priors:base_extractiveness(nash_equilibrium_coordination, 0.5).

% Suppression score (0.0-1.0)
% Rationale: 0.4. It suppresses the visibility of "idealistic" cooperation 
% by rendering it "irrational" or "unstable" from an individual perspective.
domain_priors:suppression_score(nash_equilibrium_coordination, 0.4).

% Enforcement: Emerges naturally from the axioms of individual utility.
domain_priors:emerges_naturally(nash_equilibrium_coordination).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(nash_equilibrium_coordination, extractiveness, 0.5).
narrative_ontology:constraint_metric(nash_equilibrium_coordination, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(nash_equilibrium_coordination, incumbents_in_stablesystems).
constraint_beneficiary(nash_equilibrium_coordination, market_designers). 
constraint_victim(nash_equilibrium_coordination, reformers_and_innovators). % They hit the "Mountain" of the status quo.
constraint_victim(nash_equilibrium_coordination, collective_utility). % Pareto-suboptimal states.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INDIVIDUAL SHOPKEEPER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Cannot single-handedly change market prices.
   WHEN: immediate - Every day the shop stays open.
   WHERE: trapped - Bound by the local competitive landscape.
   SCOPE: local - Their specific street or neighborhood.
   
   WHY THIS CLASSIFICATION:
   For the shopkeeper, the market price is a natural law. If they raise prices 
   unilaterally, they lose customers; if they lower them, they lose profit. 
   The equilibrium is an unyielding Mountain of arithmetic.
   
   NARRATIVE EVIDENCE:
   "I'd love to pay my staff more, but if I do, the guy across the street 
   undercuts me and I go bust." 
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    nash_equilibrium_coordination,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE CENTRAL BANKER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to shift incentives and interest rates.
   WHEN: biographical - Planning the decadal health of the economy.
   WHERE: mobile - Can adjust the "rules of the game" to nudge the equilibrium.
   SCOPE: national - Country-wide impact.
   
   WHY THIS CLASSIFICATION:
   For the institutional actor, the Nash Equilibrium is a "Rope"—a tool for 
   functional coordination. By understanding where the "Mountain" sits, they 
   can use policy to "pull" the economy toward a standard of achievement 
   like low inflation or high employment.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    nash_equilibrium_coordination,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE POVERTY-TRAPPED COMMUNITY - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the high cost of defecting from the trap.
   WHEN: generational - The state persists across lifetimes.
   WHERE: constrained - Physical and social exit is prohibitively expensive.
   SCOPE: regional - A specific neglected section of the system.
   
   WHY THIS CLASSIFICATION:
   In a poverty trap, the equilibrium acts as a "Noose." It "strangles" 
   opportunity because any individual attempt to escape (e.g., getting an 
   education) might be punished by immediate loss of local support networks. 
   It extracts potential (extraction) to maintain a stable but miserable state.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    nash_equilibrium_coordination,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(generational),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :- 
    domain_priors:base_extractiveness(nash_equilibrium_coordination, E),
    E >= 0.4,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(nash_equilibrium_tests).

test(multi_perspective_variance) :-
    % Shopkeeper -> Mountain
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, Type1, context(individual_powerless, immediate, trapped, local)),
    % Banker -> Rope
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, Type2, context(institutional, biographical, mobile, national)),
    % Poverty Trap -> Noose
    constraint_indexing:constraint_classification(nash_equilibrium_coordination, Type3, context(individual_powerless, generational, constrained, regional)),
    Type1 \= Type2,
    Type2 \= Type3.

test(extraction_penalty) :-
    % Demonstrates that the Noose classification extracts more value than the Rope.
    domain_priors:base_extractiveness(nash_equilibrium_coordination, E),
    E > 0.3.

test(natural_emergence) :-
    domain_priors:emerges_naturally(nash_equilibrium_coordination).

:- end_tests(nash_equilibrium_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.5):
 * I chose 0.5 because equilibria are the ultimate "tax" on potential. 
 * They prioritize "Stasis" over "Improvement," which is a form of 
 * extraction of future possibility.
 * * 2. PERSPECTIVE SELECTION:
 * Chose Shopkeeper (Subject), Banker (Institution), and Poverty Trap (Victim) 
 * to illustrate how the "Stability" of a Nash state can be either 
 * a comfort or a cage.
 * * 3. OMEGA IDENTIFICATION:
 * Formalized the uncertainty of "Equilibrium Selection"—in games with 
 * multiple equilibria, what determines which Mountain we live on?
 */

% YOUR OMEGAS HERE:
omega_variable(
    equilibrium_selection_mechanism,
    "What determines which specific Nash Equilibrium a society settles into (Mountain) when multiple are available (Rope)?",
    resolution_mechanism("Study of historical focal points, cultural path-dependency, and initial conditions."),
    impact("If Path-dependent: The Mountain is a 'Noose' of history. If Random: It is a Scaffold."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Pareto Optimality (Collective Cooperation)
 * Viability: Theoretically the best outcome for everyone.
 * Suppression: Actively suppressed by the Nash "Mountain" because it is 
 * unstable without external enforcement (contracts/law).
 * * ALTERNATIVE 2: Evolutionary Stable Strategies (ESS)
 * Viability: A more biological/dynamic version of Nash.
 * Status: A "Rope" used in biology that often overlaps with Nash but 
 * allows for more "drift."
 * * CONCLUSION:
 * The existence of "Pareto Superior" states (Alternative 1) that we cannot 
 * reach is exactly what transforms a Nash "Mountain" into a "Noose" for 
 * the people living within it.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_nash_equilibrium].
 * 2. Analyze: ?- multi_index_report(nash_equilibrium_coordination).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
