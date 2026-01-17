% ============================================================================
% CONSTRAINT STORY: optimal_stopping_marriage
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: The Secretary Problem / Optimal Stopping Theory
% ============================================================================

:- module(constraint_optimal_stopping_marriage, []).

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
 * * constraint_id: optimal_stopping_marriage
 * human_readable: The 37% Rule (Optimal Stopping)
 * domain: technological/mathematical
 * temporal_scope: Immediate to Biographical
 * spatial_scope: Global (Abstract Math)
 * * SUMMARY:
 * In the "Marriage Problem" (or Secretary Problem), a person must choose the best 
 * candidate from a sequence of $N$ options. Once a candidate is rejected, they cannot 
 * be recalled. To maximize the probability of picking the absolute best, math 
 * dictates rejecting the first $n/e$ (approx. 37%) of candidates and then picking 
 * the first one better than all seen so far.
 * * KEY AGENTS:
 * - The Searcher: Individual seeking the optimal partner under time/info constraints.
 * - The Candidates: Sequential options who are either accepted or permanently lost.
 * - The Mathematician: Analytical observer who views the constraint as an immutable law.
 * * NARRATIVE ARC:
 * The constraint functions as a "wall of regret." If the Searcher stops too early, 
 * they miss the global maximum. If they stop too late, the best candidate has 
 * already passed. The math creates a rigid boundary for optimal behavior.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

% Required for system integration
narrative_ontology:interval(optimal_stopping_marriage, 0, 10).
narrative_ontology:constraint_claim(optimal_stopping_marriage, mathematical_limit).

% Base extractiveness: Low (0.1). 
% Nature/Math doesn't "profit" from the Searcher, but the "loss" of rejected 
% candidates represents a form of structural waste.
domain_priors:base_extractiveness(optimal_stopping_marriage, 0.1).

% Suppression: Moderate (0.4). 
% Humans often ignore the math in favor of "gut feeling," but the math 
% "punishes" this with lower success probabilities.
domain_priors:suppression_score(optimal_stopping_marriage, 0.4).

% Enforcement: Emerges naturally from the logic of probability.
domain_priors:emerges_naturally(optimal_stopping_marriage).

% Beneficiaries: The "Winner" (the candidate selected by the optimal strategy).
constraint_beneficiary(optimal_stopping_marriage, selected_optimal_candidate).

% Victims: The "37% Group" (qualified candidates rejected solely to set the baseline).
constraint_victim(optimal_stopping_marriage, rejected_baseline_candidates).

% Metrics
narrative_ontology:constraint_metric(optimal_stopping_marriage, extractiveness, 0.1).
narrative_ontology:constraint_metric(optimal_stopping_marriage, suppression_requirement, 0.4).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SEARCHER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (subject to the sequence of options)
   WHEN: biographical (the search defines a major life phase)
   WHERE: trapped (cannot go back in time to "un-reject" a candidate)
   SCOPE: local (their specific dating pool)
   
   WHY THIS CLASSIFICATION:
   The searcher feels "strangled" by the inability to recall past options. 
   The 37% rule feels like a Noose because it forces the rejection of 
   potentially great partners simply to satisfy a statistical baseline.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    optimal_stopping_marriage,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(optimal_stopping_marriage, E),
    E < 0.2, % It's not about extraction, but the 'trapped' nature makes it a Noose.
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MATHEMATICIAN - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical/civilizational
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the mathematician, the $1/e$ limit is a "Mountain"—a fixed feature 
   of the universe like gravity. It is unchangeable regardless of human 
   desire or agency.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    optimal_stopping_marriage,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(optimal_stopping_marriage),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MATCHMAKER - Rope
   --------------------------------------------------------------------------
   WHO: institutional/individual_powerful
   WHEN: generational
   WHERE: mobile
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For an institution (like a dating app or a village matchmaker), this 
   mathematical reality is a "Rope." It's a coordination tool that 
   helps structure the flow of candidates to ensure the highest 
   aggregate "win" rate across a population.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    optimal_stopping_marriage,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    true.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(optimal_stopping_marriage_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(optimal_stopping_marriage, Type1, context(agent_power(individual_powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(optimal_stopping_marriage, Type2, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2.

test(time_immutability_shift) :-
    % Short term (biographical) feels like a Noose due to regret.
    % Long term (civilizational) is a Mountain of probability.
    constraint_indexing:constraint_classification(optimal_stopping_marriage, noose, context(_, time_horizon(biographical), trapped, _)),
    constraint_indexing:constraint_classification(optimal_stopping_marriage, mountain, context(_, time_horizon(civilizational), _, _)).

test(extraction_paradox) :-
    % The "powerless" actually experience more "loss" (perceived extractiveness) 
    % than the institutional observer.
    domain_priors:base_extractiveness(optimal_stopping_marriage, Score),
    Score < 0.5.

:- end_tests(optimal_stopping_marriage_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS: Set to 0.1. The "Marriage Problem" isn't a scam; it's a 
 * logical trap. The only "extraction" is the opportunity cost paid by 
 * the baseline group.
 * 2. PERSPECTIVES: I chose the Searcher (Noose) because the "no-recall" rule 
 * is a classic constraint that feels punitive to the individual.
 * 3. AMBIGUITIES: The biggest uncertainty is the "no-recall" assumption. 
 * In the real world, you can sometimes go back to an ex.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    recall_viability,
    "Can a previously rejected candidate be 're-activated'?",
    resolution_mechanism("Empirical study of 'boomerang' relationships in modern dating datasets"),
    impact("If Recall=High: Mountain becomes Rope (strategy changes). If Recall=Zero: Noose/Mountain holds."),
    confidence_without_resolution(medium)
).

omega_variable(
    candidate_distribution_knowledge,
    "Does the searcher know the total number of candidates $N$?",
    resolution_mechanism("Information theory analysis of search under unknown population size"),
    impact("If $N$ is unknown: The 37% rule cannot be applied; a different 'Mountain' appears."),
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Post-Selection Recall
 * Viability: High in modern social networks.
 * Suppression: The math problem suppresses this to maintain the "clean" 1/e proof.
 * * CONCLUSION:
 * The "No-Recall" rule is what turns a coordination tool (Rope) into a 
 * psychological Noose for the individual searcher.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
