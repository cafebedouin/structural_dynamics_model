% ============================================================================
% CONSTRAINT STORY: helsinki_bus_theory
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Oliver Burkeman / Arno Minkkinen, "Helsinki Bus Station Theory"
% ============================================================================

:- module(constraint_helsinki_bus_theory, []).

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
 * * constraint_id: helsinki_bus_theory
 * human_readable: The Helsinki Bus Station Theory (Creative Persistence)
 * domain: social/technological/artistic
 * temporal_scope: Biographical (Career lifespan)
 * spatial_scope: Local (The discipline) to Global (Creative output)
 * * SUMMARY:
 * This constraint defines the early stages of a creative career as an inescapable 
 * period of imitation and convergence. All "bus lines" (career paths) take 
 * the same route out of the city for the first few "stops" (years), meaning 
 * nascent work will inevitably look like the work of others. 
 * The constraint forces a choice: reset the career and return to the station, 
 * or "stay on the bus" until the route diverges into unique territory.
 * * KEY AGENTS:
 * - The Novice Artist: The agent at the first few stops, suffering from 
 * comparisons to established work.
 * - The Established Master: The agent who has "stayed on the bus" long enough 
 * to reach unique stops where they are no longer compared to others.
 * - The Critic/Market: The institutional force that judges nascent work 
 * against existing "bus lines" and induces the desire to reset.
 * * NARRATIVE ARC:
 * The novice picks a direction but finds their work is "identical" to others' 
 * early on. They are tempted to return to the station and 
 * start over, only to find the same "identical stops" on the new line. 
 * Mastery is achieved by enduring the extraction of early-career identity 
 * through persistent repetition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(helsinki_bus_theory, 0, 10).

% Updated to valid v3.1 ontology pillar: [rope]
narrative_ontology:constraint_claim(helsinki_bus_theory, rope).

% Base extractiveness: Moderate (0.4)
% Rationale: The system extracts years of labor and "uniqueness" from the 
% novice, offering only "comparison to others" in return during the initial 
% phase.
domain_priors:base_extractiveness(helsinki_bus_theory, 0.4).

% Suppression: Moderate-High (0.6)
% Rationale: The similarity to others' work suppresses the agent's belief in 
% their own originality, creating a "temptation" to abandon the path.
domain_priors:suppression_score(helsinki_bus_theory, 0.6).

% Enforcement: Emerges naturally from the structure of learning and markets.
domain_priors:emerges_naturally(helsinki_bus_theory).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(helsinki_bus_theory, extractiveness, 0.4).
narrative_ontology:constraint_metric(helsinki_bus_theory, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(helsinki_bus_theory, mastery_attainment). % Future-self who reaches the unique stops.
constraint_victim(helsinki_bus_theory, early_career_originality). % The novice's sense of unique identity.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NOVICE (YEAR 3) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Cannot skip the "identical stops")
   WHEN: immediate (The 3rd stop/3rd year of work)
   WHERE: trapped (Caught in a "nascent body of work" that looks like others')
   SCOPE: local (The specific creative field)
   
   WHY THIS CLASSIFICATION:
   For the beginner, the theory is a "Noose." They are "always being compared to others" 
   and feel that their time is being wasted in a derivative loop, strangling their 
   sense of creative self-worth.
   
   NARRATIVE EVIDENCE:
   "You pick a career direction... Three stops later... you’ll be tempted to go 
   back to the main station... you’ll still be identical".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    helsinki_bus_theory,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(helsinki_bus_theory, E),
    E > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE VETERAN MASTER (THE MINKKINEN) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "stay on the fucking bus")
   WHEN: biographical (The span of a creative life)
   WHERE: mobile (Reaching the point where the routes "diverge")
   SCOPE: global (Total career output)
   
   WHY THIS CLASSIFICATION:
   To the master, the constraint is a "Rope." It is the functional coordination 
   mechanism that leads to uniqueness. Staying on the path is the "secret" to 
   a fulfilling career, turning repetition into a tool for breakthrough.
   
   NARRATIVE EVIDENCE:
   "Stay on the bus. Stay on the fucking bus... Thereafter... all the lines... 
   take a different route".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    helsinki_bus_theory,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the "operations of Helsinki’s main bus station")
   WHEN: historical (The perennial pattern of skill acquisition)
   WHERE: analytical (Observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the observer, this is a "Mountain"—an unchangeable structural reality of 
   how mastery works. One cannot simply "leap" to uniqueness; the first kilometer 
   is a zero-degree-of-freedom path that all must traverse.
   
   NARRATIVE EVIDENCE:
   "all the lines leaving from any one platform take the same route out of the 
   city... This goes on all your creative life".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    helsinki_bus_theory,
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

:- begin_tests(helsinki_bus_theory_tests).

test(multi_perspective_persistence) :-
    % Novice sees Noose
    constraint_indexing:constraint_classification(helsinki_bus_theory, Type1, context(individual_powerless, immediate, trapped, local)),
    % Master sees Rope
    constraint_indexing:constraint_classification(helsinki_bus_theory, Type2, context(individual_moderate, biographical, mobile, global)),
    % Observer sees Mountain
    constraint_indexing:constraint_classification(helsinki_bus_theory, Type3, context(analytical, historical, analytical, global)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_persistence) :-
    % The novice at year 3 loses more "identity" than the master who gained it.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, global),
    constraint_indexing:extractiveness_for_agent(helsinki_bus_theory, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(helsinki_bus_theory, ContextModerate, Score2),
    Score1 > Score2.

test(time_immutability_persistence) :-
    % Short-term imitation feels unchangeable (Mountain/Noose).
    % Long-term persistence allows for a shift in Type (Rope).
    constraint_indexing:effective_immutability(historical, analytical, mountain).

:- end_tests(helsinki_bus_theory_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: I chose a moderate score because while the theory is ultimately 
 * beneficial (mastery), it extracts a high "tax" in the form of early-career 
 * identity and the "year stops" where one's work is not yet uniquely theirs 
 *. 
 * * 2. SUPPRESSION SCORE (0.6):
 * Reasoning: The theory explicitly notes that "being compared to others" 
 * is the mechanism that induces people to leave the path. 
 * Originality is suppressed by the structural commonality of the route.
 * * 3. PERSPECTIVE SELECTION:
 * The Novice (Noose) highlights the feeling of being derivative; the 
 * Master (Rope) highlights persistence as a tool; the Observer (Mountain) 
 * highlights the structural inevitability.
 * * 4. AMBIGUITIES:
 * - The "divergence point" is not strictly defined in time. This is 
 * resolved by the biographical time horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divergence_point_certainty,
    "Does every 'bus line' truly diverge into unique territory, or do some lines remain derivative indefinitely (a permanent Noose)?",
    resolution_mechanism("Audit of creative career outcomes across divergent disciplines vs. persistent imitation rates"),
    impact("If some lines never diverge: The theory is a Noose for those lines. If all diverge: It is a true Rope."),
    confidence_without_resolution(medium)
).

omega_variable(
    originality_as_myth,
    "Is 'uniqueness' at the end of the route an objective Mountain, or a narrative Rope created to justify the persistence?",
    resolution_mechanism("Aesthetic and informational comparison of 'unique territory' work against the 'station' archives"),
    impact("If myth: The entire system is a deceptive Noose for time-extraction. If real: The theory holds as a Mountain/Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: The "New Station" Strategy (Resetting)
 * Viability: Frequently chosen by those who "go back to the main station".
 * Suppression: Actively discouraged by the theory as it leads back to the 
 * same "identical stops".
 * * ALTERNATIVE 2: Off-Route Exploration (Non-Bus)
 * Viability: The idea of skipping the foundational imitation phase.
 * Suppression: Presented as structurally impossible; the "lines take the same 
 * route" is a fixed feature of the "station".
 * * CONCLUSION:
 * The theory functions by suppressing the viability of Alternative 1. By 
 * labeling resetting as a failure to move past the "identical" stage, it 
 * transforms the "Noose" of imitation into the "Rope" of persistence.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_helsinki_bus_theory].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(helsinki_bus_theory).
 * 3. Run tests: ?- run_tests(helsinki_bus_theory_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
