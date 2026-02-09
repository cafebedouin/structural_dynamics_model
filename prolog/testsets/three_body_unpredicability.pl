% ============================================================================
% CONSTRAINT STORY: three_body_unpredictability
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Isaac Newton (1687) / Henri Poincaré (1887) / Celestial Mechanics
% ============================================================================

:- module(constraint_three_body, []).

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
 * * constraint_id: three_body_unpredictability
 * human_readable: The Three-Body Problem (Orbital Chaos)
 * domain: technological/scientific
 * temporal_scope: 1687 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Astrophysics)
 * * SUMMARY:
 * The Three-Body Problem involves calculating the motion of three celestial bodies 
 * interacting via gravity. Unlike the stable two-body system, the three-body 
 * system generally lacks a general closed-form solution and exhibits extreme 
 * sensitivity to initial conditions (chaos), rendering long-term trajectories 
 * uncomputable.
 * * KEY AGENTS:
 * - The Celestial Body (Subject): A powerless agent whose path is dictated by 
 * the intersection of two or more gravitational wells.
 * - The Orbital Engineer (Institutional): An agent who uses specific stable 
 * subsets (Lagrange points) as a "Rope" to station satellites.
 * - The Navigator/Predictor (Victim): An agent for whom the chaotic divergence 
 * acts as a "Snare," extracting the possibility of long-term survival or 
 * trajectory certainty.
 * * NARRATIVE ARC:
 * The Three-Body Problem is the "Mountain" of celestial mechanics; gravity is 
 * unyielding, yet its complexity prevents a total mapping. In space exploration, 
 * Lagrange points are the "Ropes" that allow for stable coordination. However, 
 * for a civilization living in a triple-star system, the lack of a stable 
 * "Mountain" of predictable seasons acts as a "Snare," extracting all 
 * developmental consistency and "strangling" the future with sudden, 
 * chaotic extinction events.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural extraction
narrative_ontology:interval(celestial_mechanics_era, 1687, 2026).
narrative_ontology:constraint_claim(three_body_unpredictability, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.4. It "extracts" the long-term utility of initial data. To 
% maintain any predictive "Rope," observers must constantly re-invest in 
% high-precision measurements, as the "Mountain" of gravity shreds 
% information over time.
domain_priors:base_extractiveness(three_body_unpredictability, 0.4).

% Suppression score (0.0-1.0)
% Rationale: 0.3. It suppresses the visibility of simple Newtonian 
% determinism in multi-body environments, rendering "clockwork universe" 
% narratives functionally fraudulent at scale.
domain_priors:suppression_score(three_body_unpredictability, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(three_body_unpredictability, extractiveness, 0.4).
narrative_ontology:constraint_metric(three_body_unpredictability, suppression_requirement, 0.3).

% Enforcement: Emerges naturally from the non-linear feedback of gravity.
domain_priors:emerges_naturally(three_body_unpredictability).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(three_body_unpredictability, chaos_theorists).
narrative_ontology:constraint_beneficiary(three_body_unpredictability, n_body_simulators).
narrative_ontology:constraint_victim(three_body_unpredictability, long_term_navigators).
narrative_ontology:constraint_victim(three_body_unpredictability, triple_star_civilizations). % Absolute extraction of stability.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PLANET IN A TRIPLE-STAR SYSTEM - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - The planet has zero agency; gravity is absolute.
   WHEN: immediate - True at every infinitesimal moment of the orbit.
   WHERE: trapped - Bound by the gravitational field.
   SCOPE: local - Immediate neighborhood of the current vector.
   
   WHY THIS CLASSIFICATION:
   For the planet, the gravitational pull of the three suns is an absolute 
   Mountain. It cannot "choose" a different path; its velocity and position 
   are dictated by a law that, while chaotic, is unyielding. The lack of 
   periodicity is a fixed geographic feature of its existence.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    three_body_unpredictability,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SATELLITE ENGINEER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to design orbits and place craft at Lagrange points.
   WHEN: biographical - Planning the 20-year lifespan of a telescope (e.g., JWST).
   WHERE: mobile - Can choose specific "islands of stability" in the system.
   SCOPE: national - Protecting state assets in space.
   
   WHY THIS CLASSIFICATION:
   For the engineer, the Three-Body Problem provides "Ropes"—the Lagrange 
   points ($L_1$ to $L_5$). By understanding the "Mountain" of gravitational 
   interaction, they coordinate a standard of achievement by placing 
   sensors in specific points where forces balance, "pulling" the system 
   into a state of functional utility.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    three_body_unpredictability,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INTERSTELLAR NAVIGATOR - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Has agency to navigate but is bound by the chaos.
   WHEN: historical - Planning a multi-century journey through a star cluster.
   WHERE: constrained - No alternative but to navigate the non-linear field.
   SCOPE: global - Across the scale of the solar system or cluster.
   
   WHY THIS CLASSIFICATION:
   For the navigator, the Three-Body Problem is a "Snare." It "strangles" 
   long-term planning. Because errors grow exponentially, the system 
   extracts fuel and compute cycles (extraction) for constant course 
   corrections, "choking" the possibility of a fixed, pre-calculated 
   interstellar highway.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    three_body_unpredictability,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(three_body_unpredictability, E),
    E >= 0.35,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(three_body_tests).

test(multi_perspective_variance) :-
    % Planet -> Mountain
    constraint_indexing:constraint_classification(three_body_unpredictability, Type1, context(powerless, immediate, trapped, local)),
    % Engineer -> Rope
    constraint_indexing:constraint_classification(three_body_unpredictability, Type2, context(institutional, biographical, mobile, national)),
    Type1 \= Type2.

test(navigation_extraction_penalty) :-
    % Navigators feel the extraction of certainty as a Snare.
    Context = context(individual_moderate, historical, constrained, global),
    constraint_indexing:extractiveness_for_agent(three_body_unpredictability, Context, Score),
    Score >= 0.4.

test(natural_emergence) :-
    domain_priors:emerges_naturally(three_body_unpredictability).

:- end_tests(three_body_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Reasoning: Chaos "extracts" information value. In a three-body system, 
 * the future state "taxes" the initial state at an exponential rate. 
 * High extractiveness for those seeking stability.
 * * 2. CLASSIFICATION RATIONALE:
 * - Planet (Mountain): The inevitable subject of physics.
 * - Engineer (Rope): Using Lagrange points to "tie" down utility.
 * - Navigator (Snare): Where unpredictability "chokes" the long-term mission.
 */

% OMEGA IDENTIFICATION
omega_variable(
    orbital_resonance_stability,
    "Is the 'Mountain' of chaos stable, or are there 'Ropes' (periodic orbits) that remain to be discovered?",
    resolution_mechanism("Numerical search for periodic solutions in the restricted three-body problem."),
    impact("If Yes: Navigators gain new Ropes. If No: The 'Snare' of chaos is total."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * * ALTERNATIVE 1: The Restricted Three-Body Problem
 * Viability: Assuming one mass is negligible (e.g., a satellite) makes the 
 * math more manageable (though still chaotic).
 * Suppression: Often presented as the "only" way to do practical engineering, 
 * suppressing the visibility of the "Full" three-body interaction.
 * * * ALTERNATIVE 2: Two-Body Approximation
 * Viability: Treating the Earth-Sun system as isolated.
 * Suppression: Replaced by the Three-Body "Mountain" because it leads to 
 * catastrophic drift in long-term models.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_three_body].
 * 2. Multi-perspective: ?- multi_index_report(three_body_unpredictability).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(three_body_unpredicability, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(three_body_unpredicability, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(three_body_unpredicability, snare, agent_power(powerless)).

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
% Technical constraint — mostly substantive, minimal implementation theater
domain_priors:theater_ratio(three_body_unpredictability, 0.0).
narrative_ontology:constraint_metric(three_body_unpredictability, theater_ratio, 0.0).
