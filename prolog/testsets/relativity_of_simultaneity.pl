% ============================================================================
% CONSTRAINT STORY: relativity_of_simultaneity
% ============================================================================
% Generated: January 17, 2026
% Model: Gemini 2.0 Flash
% Source: Relativity: The Special and General Theory by Albert Einstein (1920)
% ============================================================================

:- module(constraint_simultaneity, []).

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
 * * constraint_id: relativity_of_simultaneity
 * human_readable: The Relativity of Simultaneity
 * domain: scientific/technological
 * temporal_scope: 1905 CE - Present
 * spatial_scope: Universal
 * * SUMMARY:
 * This constraint establishes that "simultaneity" is not an absolute property of 
 * the universe but is dependent on the motion of the reference body. 
 * Events that are simultaneous relative to an embankment are not simultaneous 
 * relative to a moving train.
 * * KEY AGENTS:
 * - Embankment Observer (M): The "stationary" agent who defines simultaneity 
 * by the meeting of light rays at a midpoint.
 * - Train Observer (M'): The "moving" agent who, by hastening toward one light 
 * beam and away from another, perceives a time sequence.
 * - The "Classical Physicist": The agent trapped in the "arbitrary" assumption 
 * that time has an absolute significance.
 * * NARRATIVE ARC:
 * Einstein identifies that the "conflict" between relativity and light propagation 
 * arises from a "tacitly assumed" absolute time. By analyzing the 
 * physical meaning of time, the constraint is shifted from an "absolute mountain" 
 * to a "relative coordinate".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(relativity_of_simultaneity, 0, 10).
narrative_ontology:constraint_claim(relativity_of_simultaneity, physical_law).

% Base extractiveness: 0.0
% Rationale: The law governs the flow of time for all observers equally; 
% there is no asymmetric "theft" of time, only a difference in measurement.
domain_priors:base_extractiveness(relativity_of_simultaneity, 0.0).

% Suppression score: 0.4
% Rationale: While not "punished," the alternative (absolute time) is deeply 
% "lodged in our habit of thought" and was "tacitly assumed" for centuries.
domain_priors:suppression_score(relativity_of_simultaneity, 0.4).

% Enforcement: Emerges naturally
% Rationale: It is a consequence of the constancy of light velocity.
domain_priors:emerges_naturally(relativity_of_simultaneity).

narrative_ontology:constraint_metric(relativity_of_simultaneity, extractiveness, 0.0).
narrative_ontology:constraint_metric(relativity_of_simultaneity, suppression_requirement, 0.0).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COMMON OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to intuitive, everyday experience)
   WHEN: immediate (Local, daily observations)
   WHERE: trapped (Deeply lodged habits of thought )
   SCOPE: local (Low velocities where relativistic effects are invisible )
   
   WHY THIS CLASSIFICATION:
   For the common person, time appears as an absolute, unchangeable "Mountain." 
   The idea that events could be simultaneous for one but not another feels 
   "occult" or "mysterious".
   
   NARRATIVE EVIDENCE:
   "It had always tacitly been assumed in physics that the statement of time 
   had an absolute significance".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    relativity_of_simultaneity,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(relativity_of_simultaneity, S),
    S > 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRAIN OBSERVER (M') - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Participant in the experiment)
   WHEN: biographical (Duration of the "event" or experiment)
   WHERE: constrained (Bound to the reference body of the train )
   SCOPE: regional (Spanning the distance between flashes A and B )
   
   WHY THIS CLASSIFICATION:
   The observer M' is "constrained" by their motion. The constraint acts as a 
   "Noose" because it forces them to conclude that B happened before A, 
   directly contradicting the "truth" of the embankment observer.
   
   NARRATIVE EVIDENCE:
   "Observers who take the railway train as their reference-body must therefore 
   come to the conclusion that the lightning flash B took place earlier than 
   the lightning flash A".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    relativity_of_simultaneity,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :-
    % Classification logic
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE RELATIVISTIC PHYSICIST - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (The "conscientiously thoughtful physicist" )
   WHEN: historical (Long-term theoretical development)
   WHERE: mobile (Conceptual mobility through the Lorentz Transformation )
   SCOPE: global (Applies to the "universe as a whole" )
   
   WHY THIS CLASSIFICATION:
   The relativity of simultaneity is a "Rope"—a tool for coordination. It 
   allows the physicist to reconcile the Principle of Relativity with the Law 
   of Light, forming a "logically rigid theory".
   
   NARRATIVE EVIDENCE:
   "The dilemma... disappears... by systematically holding fast to both these 
   laws".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    relativity_of_simultaneity,
    rope,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(relativity_of_simultaneity, E),
    E == 0.0,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(relativity_of_simultaneity_tests).

/**
 * TEST 1: The Disappearance of Absolute Time
 * Demonstrates that moving from powerless/trapped to analytical/mobile 
 * changes the constraint from a Mountain to a Rope.
 */
test(multi_perspective_simultaneity) :-
    constraint_indexing:constraint_classification(relativity_of_simultaneity, mountain, 
        context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(relativity_of_simultaneity, rope, 
        context(analytical, historical, mobile, global)).

/**
 * TEST 2: Conflict Recognition
 * Demonstrates that the "moderate power" observer (M') experiences a "Noose" 
 * because their experience conflicts with another "truth" (M).
 */
test(noose_conflict) :-
    constraint_indexing:constraint_classification(relativity_of_simultaneity, noose, 
        context(individual_moderate, biographical, constrained, regional)).

:- end_tests(relativity_of_simultaneity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. SUPPRESSION SCORE (0.4): I rated this higher than the velocity of light 
 * because Einstein explicitly notes how "deeply lodged" the Euclidean/Newtonian 
 * view of time is in the human psyche. 
 * * 2. PERSPECTIVE 2 (Noose): In this context, "Noose" represents the 
 * perceptual "trap" where two observers are forced into mutually 
 * exclusive conclusions ("B before A" vs "Simultaneous").
 * * 3. ANALYTICAL MOBILITY: The physicist reaches "Rope" status not by 
 * changing the law, but by changing the coordinate system (Lorentz 
 * Transformation) to account for it.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Absolute Simultaneity (Newtonian) 
 * Viability: Historically dominant; matches "everyday life".
 * Suppression: Rejected because it is "incompatible" with the law of 
 * light propagation.
 * * CONCLUSION:
 * The "Rope" of relative simultaneity is chosen because the alternative 
 * (Absolute Time) creates a logical "Noose" for the laws of physics.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Multi-perspective: ?- multi_index_report(relativity_of_simultaneity).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
