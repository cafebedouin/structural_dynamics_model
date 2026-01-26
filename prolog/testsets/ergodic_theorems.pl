% ============================================================================
% CONSTRAINT STORY: ergodic_theorems
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Statistical Mechanics / Probability Theory / Ergodicity
% ============================================================================

:- module(constraint_ergodic_theorems, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ergodic_theorems
 * human_readable: Ergodic Theorems (Time vs. Ensemble Averages)
 * domain: scientific/mathematical/economic
 * temporal_scope: Universal (Physics and Decision Theory)
 * spatial_scope: Universal (Stochastic Systems)
 * * SUMMARY:
 * Ergodic theorems describe systems where the "ensemble average" (the average 
 * across many parallel states) equals the "time average" (the average of one 
 * state over a long period). In non-ergodic systems, such as gambling with 
 * ruin or evolutionary survival, the "ensemble" success rate is irrelevant 
 * because a single path hitting zero results in total termination.
 * * KEY AGENTS:
 * - The Statistician: Assumes ergodicity to simplify calculations of large 
 * populations.
 * - The Individual Gambler: Faces the "ruin" problem where time averages 
 * diverge from ensemble expectations.
 * - The Biological Organism: Subject to the non-ergodic constraint of 
 * mortality—you cannot "average" your life with others if you die.
 * * NARRATIVE ARC:
 * Ergodicity is often treated as a "Rope" for calculation by institutions 
 * (Insurers, Economists), but for the individual subject to ruin, it is an 
 * invisible "Mountain" of physical limitation. When systems are designed 
 * as if they were ergodic when they are not, they become a "Snare" for the 
 * participants who are "absorbed" (eliminated) by volatility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(ergodic_interval, 0, 10).
narrative_ontology:constraint_claim(ergodic_theorems, mountain).

% Base extractiveness: 0.3 (Moderate)
% Rationale: While ergodicity is a physical/mathematical property, the 
% *assumption* of ergodicity extracts value by justifying risky policies 
% (e.g., in finance or public health) that work on average but fail individuals.
domain_priors:base_extractiveness(ergodic_theorems, 0.3).

% Suppression: 0.6 (High)
% Rationale: Non-ergodicity is frequently suppressed in mainstream economic 
% modeling. The "expected value" (ensemble) is presented as the only rational 
% choice, hiding the risk of individual termination.
domain_priors:suppression_score(ergodic_theorems, 0.6).

% Enforcement: Emerges naturally from the nature of probability and absorbing barriers.
domain_priors:emerges_naturally(ergodic_theorems).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(ergodic_theorems, extractiveness, 0.3).
narrative_ontology:constraint_metric(ergodic_theorems, suppression_requirement, 0.6).

% Beneficiaries: Institutional planners, insurers, and high-frequency traders 
% who can play the ensemble.
constraint_beneficiary(ergodic_theorems, institutional_aggregators).

% Victims: Individual actors, small businesses, and biological entities 
% subject to path-dependency and ruin.
constraint_victim(ergodic_theorems, path_dependent_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MATHEMATICAL PHYSICIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The observer of phase space).
   WHEN: civilizational (Eternal laws of statistical mechanics).
   WHERE: trapped (Cannot escape the definitions of measure theory).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   To the physicist, the ergodic hypothesis is a Mountain. It defines the 
   boundary of what can be calculated. Systems are either ergodic or they 
   are not; this is an objective property of the system's phase space 
   and dynamics. It is the "ground" upon which thermodynamics is built.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergodic_theorems,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INSURANCE ACTUARY - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (The manager of large-scale risk).
   WHEN: historical (Scaling over the life of an institution).
   WHERE: arbitrage (Can aggregate across millions of individuals).
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For the actuary, ergodicity is a Rope. By pooling enough individuals, 
   the "time average" of the insurer becomes the "ensemble average" of 
   the population. It is a coordination mechanism that allows for the 
   monetization of risk and the creation of social stability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergodic_theorems,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE INDIVIDUAL GAMBLER - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A single agent with one "life").
   WHEN: immediate (The current sequence of bets).
   WHERE: trapped (Once 'ruin' occurs, no more play is possible).
   SCOPE: local.
   
   WHY THIS CLASSIFICATION:
   For the individual facing a non-ergodic game, the "average" is a Snare. 
   If the game has a 99% success rate but the 1% failure results in death 
   (ruin), the "ensemble average" (thriving) is irrelevant. The structure 
   of the game strangles the player over time, as the probability of 
   hitting the absorbing barrier approaches 1.0.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    ergodic_theorems,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(ergodic_theorems_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ergodic_theorems, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(ergodic_theorems, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ergodic_theorems, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_scaling) :-
    % Institutional actors benefit from aggregation; individuals suffer from ruin.
    domain_priors:base_extractiveness(ergodic_theorems, E),
    E >= 0.3.

test(time_immutability) :-
    % Civilizational horizon views mathematical laws as Mountains.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(ergodic_theorems_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS (0.3):
 * The score is moderate because ergodicity itself is a law of nature, 
 * but its misapplication by "Expert" institutions (The Fragilistas) 
 * extracts survival and capital from the individual.
 * * 2. SUPPRESSION (0.6):
 * High because traditional economics almost entirely ignores non-ergodicity 
 * (the 'ruin' problem), effectively suppressing the most important 
 * constraint on individual survival.
 * * 3. CLASSIFICATION RATIONALE:
 * Physicist (Mountain): The theorem is a tautological truth of math.
 * Actuary (Rope): Institutional scaling converts ergodicity into a tool.
 * Gambler (Snare): Path-dependence and mortality turn probabilities 
 * into terminal traps.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_ergodicity,
    "Do quantum systems truly explore all available phase space (Ergodic) or are they subject to fundamentally discrete, non-ergodic jumps?",
    resolution_mechanism("Long-term observation of decoherence in complex quantum states"),
    impact("If Mountain: Physics is unified. If Snare: The universe has 'dead-ends' in its probability structure."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Multiplicative Dynamics (Kelly Criterion)
 * Viability: A proven mathematical strategy for navigating non-ergodic 
 * systems by betting a fraction of wealth to avoid ruin.
 * Suppression: Moderate. Taught in niche finance/gambling but ignored 
 * in general economic policy which favors "expected value."
 * * CONCLUSION:
 * The existence of the Kelly Criterion as an alternative shifts the 
 * "Snare" of ergodicity into a "Rope" for those with the knowledge and 
 * power to apply it.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_ergodic_theorems].
 * 2. Multi-perspective: ?- multi_index_report(ergodic_theorems).
 * 3. Run tests: ?- run_tests(ergodic_theorems_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
