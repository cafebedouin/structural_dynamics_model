% ============================================================================
% CONSTRAINT STORY: helsinki_bus_theory
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
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
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: helsinki_bus_theory
 * human_readable: The Helsinki Bus Station Theory (Creative Persistence)
 * domain: artistic/social/psychological
 * temporal_scope: Biographical (Career lifespan)
 * spatial_scope: Local (The discipline) to Global (Creative output)
 * 
 * SUMMARY:
 * This constraint defines the early stages of a creative career as an inescapable 
 * period of imitation and convergence. All "bus lines" (career paths) take 
 * the same route out of the city for the first few "stops" (years), meaning 
 * nascent work will inevitably look like the work of others. 
 * The constraint forces a choice: reset the career and return to the station, 
 * or "stay on the bus" until the route diverges into unique territory.
 * 
 * KEY AGENTS:
 * - The Novice Artist (Individual Powerless): At the first few stops, suffering from comparisons.
 * - The Art School / Record Label (Institutional): Manages talent development through this phase.
 * - The Established Master (Individual Moderate): Has "stayed on the bus" long enough to reach unique territory.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (Context-Independent)
   ========================================================================== */

narrative_ontology:interval(helsinki_bus_theory, 0, 10).
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

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(helsinki_bus_theory, extractiveness, 0.4).
narrative_ontology:constraint_metric(helsinki_bus_theory, suppression_requirement, 0.6).

% Enforcement: Emerges naturally from the structure of learning and markets.
domain_priors:emerges_naturally(helsinki_bus_theory).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(helsinki_bus_theory, established_masters).
constraint_victim(helsinki_bus_theory, novice_artists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NOVICE ARTIST - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Cannot skip the "identical stops")
   WHEN: immediate (The early years of work)
   WHERE: trapped (Caught in a "nascent body of work" that looks like others')
   
   WHY THIS CLASSIFICATION:
   For the beginner, the theory is a 'Snare'. They are "always being compared to others" 
   and feel that their time is being wasted in a derivative loop, strangling their 
   sense of creative self-worth and leading to frustration.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    helsinki_bus_theory,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ART SCHOOL / RECORD LABEL - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Manages talent development)
   WHEN: biographical (Developing a career over years)
   WHERE: mobile (Can choose which artists to support)
   
   WHY THIS CLASSIFICATION:
   For an institution managing talent, the theory is a 'Rope'. It provides a framework
   to understand that early work will be derivative. They can use it to guide artists,
   foster persistence, and identify those with the resilience to reach unique territory.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    helsinki_bus_theory,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ESTABLISHED MASTER - Mountain
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has the agency to "stay on the fucking bus")
   WHEN: biographical (The span of a creative life)
   WHERE: mobile (Reaching the point where the routes "diverge")
   
   WHY THIS CLASSIFICATION:
   To the master, the constraint is a 'Mountain'—an unchangeable structural reality of 
   how mastery works. One cannot simply "leap" to uniqueness; the first kilometer 
   is a zero-degree-of-freedom path that all must traverse.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    helsinki_bus_theory,
    mountain,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(helsinki_bus_theory_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(helsinki_bus_theory, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(helsinki_bus_theory, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(helsinki_bus_theory, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(helsinki_bus_theory_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'Art School / Record Label' to represent
 *    the institutional agent. This provides a clear classification of 'Rope'
 *    for an entity that uses the theory to manage and develop talent.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Novice (Snare): Trapped in the derivative phase, risking self-worth.
 *    - Art School (Rope): A tool for talent management and expectation setting.
 *    - Master (Mountain): The inevitable, unchanging path to mastery.
 * 
 * 3. EXTRACTIVENESS (0.4): Moderate, as it extracts early-career identity and
 *    time. The frustration of feeling derivative is a real cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether uniqueness is an objective outcome or a subjective belief.
 */

omega_variable(
    divergence_point_certainty,
    "Does every 'bus line' truly diverge into unique territory, or do some lines remain derivative indefinitely (a permanent Snare)?",
    resolution_mechanism("Longitudinal qualitative studies of creative careers across disciplines, tracking objective output uniqueness vs. subjective artist satisfaction."),
    impact("If some lines never diverge: The theory is a Snare for those lines. If all diverge: It is a true Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: The "New Station" Strategy (Resetting Careers)
 *    Viability: Frequently chosen by those who "go back to the main station" and try a new path.
 *    Suppression: Actively discouraged by the theory as it leads back to the same "identical stops"
 *    and delays true mastery.
 * 
 * CONCLUSION:
 * The theory functions by suppressing the viability of resetting one's career. By 
 * labeling resetting as a failure to move past the "identical" stage, it 
 * transforms the "Snare" of early-career frustration into the "Rope" of persistence.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/helsinki_bus_theory].
 * 2. Multi-perspective: ?- multi_index_report(helsinki_bus_theory).
 * 3. Run tests: ?- run_tests(helsinki_bus_theory_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Coordination mechanism in artistic domain — moderate institutional framing
domain_priors:theater_ratio(helsinki_bus_theory, 0.12).
narrative_ontology:constraint_metric(helsinki_bus_theory, theater_ratio, 0.12).

% --- Analytical perspective classification (missing) ---
% chi = 0.4 * 1.15 (analytical) * 1.2 (global) = 0.552
% Classification: tangled_rope
constraint_indexing:constraint_classification(helsinki_bus_theory, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
