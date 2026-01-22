% ============================================================================
% CONSTRAINT STORY: asshole_filter_2015
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "[psych/anthro, Patreon] The Asshole Filter" by siderea
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(asshole_filter_2015, []).

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
 * * constraint_id: asshole_filter_2015
 * human_readable: The Asshole Filter
 * domain: psychological/social
 * temporal_scope: 2015-present
 * spatial_scope: Organizational/Social environments
 * * SUMMARY:
 * An "asshole filter" is a systemic phenomenon where established norms or 
 * boundaries unintentionally repel non-transgressive people while attracting 
 * and rewarding transgressive ones. This occurs when a 
 * rule is publicly stated but not enforced, or worse, those who break it 
 * are rewarded with faster or better service.
 * * KEY AGENTS:
 * - The Weak Enforcer (Fred): The rule-setter who fails to defend his 
 * own boundaries.
 * - The Rule-Follower (Non-Asshole): Respects the stated boundary and is 
 * subsequently penalized or ignored.
 * - The Transgressor (Asshole): Ignores the boundary and is rewarded, 
 * reinforcing their transgressive behavior.
 * * NARRATIVE ARC:
 * A boundary is set (e.g., "don't email my personal account"). Respectful 
 * people obey and wait in a slow queue. Transgressive 
 * people ignore the rule, email directly, and receive immediate attention 
 *. The "filter" drains away the polite people, leaving 
 * the enforcer surrounded only by assholes.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(asshole_filter_2015, 0, 10).
narrative_ontology:constraint_claim([asshole_filter_2015], [social_filter]).

% Base extractiveness score (0.75 = High)
% Rationale: The system extracts compliance and patience from decent people 
% while transferring rewards (time, attention) to rule-breakers. This creates 
% a severe asymmetry where the "well-behaved" are systematically disadvantaged 
%.
domain_priors:base_extractiveness(asshole_filter_2015, 0.75).

% Suppression score (0.6 = Moderate-High)
% Rationale: Decent behavior is suppressed because it becomes ineffective. 
% The alternative of "being polite" is invisible to the enforcer because 
% those people have been filtered out of his immediate experience.
domain_priors:suppression_score(asshole_filter_2015, 0.6).

% Enforcement requirements
% Requires active enforcement to *avoid* being an asshole filter. 
% The filter itself "emerges naturally" when enforcement of boundaries fails.
domain_priors:requires_active_enforcement(asshole_filter_2015).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(asshole_filter_2015, extractiveness, 0.75).
narrative_ontology:constraint_metric(asshole_filter_2015, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(asshole_filter_2015, transgressive_agents).
constraint_victim(asshole_filter_2015, rule_following_agents).
constraint_victim(asshole_filter_2015, the_agreeable_enforcer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RULE-FOLLOWER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Respects the rules but cannot enforce them)
   WHEN: immediate (Experiencing delays and unfairness now)
   WHERE: trapped (Committed to social decency/rules)
   SCOPE: local (Their specific interaction/request)
   
   WHY THIS CLASSIFICATION:
   For the decent person, the norm is a "Noose." They follow it and are 
   penalized (screwed) while watching transgressors get rewarded. 
   The rule constrains only those who are willing to be constrained.
   
   NARRATIVE EVIDENCE:
   "when you set up a system whereby the honest, rule-following people get 
   screwed and the transgressors are rewarded... [they] will be wicked pissed" 
  .
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    asshole_filter_2015,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(asshole_filter_2015, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TRANSGRESSOR (ASSHOLE) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to ignore rules)
   WHEN: immediate (Getting their needs met now)
   WHERE: mobile (Will find any "shortcut" or "illicit" path)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   The transgressor sees the rule as a "Rope." It is a coordination mechanism 
   that they use as a shortcut. It clears the "suckers" out of the way, making 
   it easier for the transgressor to get direct access to the enforcer 
  .
   
   NARRATIVE EVIDENCE:
   "People who feel satisfaction when they find an illicit 'shortcut'... that 
   'suckers' are too 'chicken' to use".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    asshole_filter_2015,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing behavioral patterns)
   WHEN: civilizational (Fundamental human behavioral mechanisms)
   WHERE: analytical (Observer stance)
   SCOPE: global (Applies to dating, hiring, and management)
   
   WHY THIS CLASSIFICATION:
   From an analytical perspective, this is a "Mountain." It is a predictive 
   pattern of operant conditioning and social dynamics that functions like 
   a law of nature if certain conditions (lack of enforcement) are met 
  .
   
   NARRATIVE EVIDENCE:
   "If you tell people 'the only way to contact me is to break a rule' you 
   will only be contacted by rule-breakers".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    asshole_filter_2015,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE WEAK ENFORCER (FRED) - Noose
   --------------------------------------------------------------------------
   
   WHO: institutional (In charge of the process)
   WHEN: biographical (Enduring a career/project of harassment)
   WHERE: constrained (Trapped by his own "agreeableness" and lack of judgment)
   SCOPE: national/global
   
   WHY THIS CLASSIFICATION:
   Fred is in a "Noose" created by his inability to enforce boundaries. He is 
   strangled by a concentrated dose of entitlement and disrespect because he 
   drained away all the pleasant interactions.
   
   NARRATIVE EVIDENCE:
   "He has relieved himself of dealing with half the work, but the half 
   that's left are the tough customers... causing him to feel like, 
   'People Suck'".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    asshole_filter_2015,
    noose,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(asshole_filter_2015_tests).

test(multi_perspective_asymmetry) :-
    % Rule-Follower (Powerless) sees Noose
    constraint_indexing:constraint_classification(asshole_filter_2015, noose, context(individual_powerless, _, _, _)),
    % Transgressor (Moderate) sees Rope
    constraint_indexing:constraint_classification(asshole_filter_2015, rope, context(individual_moderate, _, _, _)),
    % Enforcer (Institutional) sees Noose (if weak)
    constraint_indexing:constraint_classification(asshole_filter_2015, noose, context(institutional, _, _, _)).

test(powerless_extraction_scaling) :-
    % Rule-followers experience more extraction (wait time + bitterness) 
    % than transgressors who get the "pellet" immediately.
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextTransgressor = context(individual_moderate, immediate, mobile, local),
    constraint_indexing:extractiveness_for_agent(asshole_filter_2015, ContextPowerless, S1),
    constraint_indexing:extractiveness_for_agent(asshole_filter_2015, ContextTransgressor, S2),
    S1 > S2.

test(time_immutability_shift) :-
    % Immediate experience = Noose (I'm stuck in the queue). 
    % Historical/Analytical view = Mountain (This is just how systems work).
    constraint_indexing:effective_immutability(immediate, trapped, mountain),
    constraint_indexing:effective_immutability(civilizational, analytical, rope).

:- end_tests(asshole_filter_2015_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * The score is high because the system relies on the "decency" of non-assholes 
 * to function (they clear the queue), but then betrays that decency by 
 * prioritizing the "rude." This is a predatory social structure.
 * * 2. MANDATROPHY RISK:
 * The constraint qualifies because the "Rope" for the transgressor is 
 * generated directly by the "Noose" applied to the rule-follower. The 
 * extraction is not an accident; it is the fundamental mechanism of the 
 * filter.
 * * 3. PERSPECTIVE SELECTION:
 * I included "Fred" as an institutional perspective because it demonstrates 
 * how the rule-maker can also be a victim of their own low-enforcement 
 * environment.
 * * 4. AMBIGUITIES:
 * - The text treats "asshole" as "transgressiveness," which simplifies 
 * complex social dynamics into a binary for the sake of the model. 
 * - I have resolved this by declaring an Omega about the multi-dimensionality 
 * of transgressiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asshole_filter_2015_extraction_intent,
    "Is the extraction of time from rule-followers a functional necessity to handle volume, or a predatory bias toward insistent agents?",
    resolution_mechanism("Audit of queue processing speed: do transgressors actually increase systemic throughput or just jump the line?"),
    impact("If necessity: Mountain. If predatory: Noose/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    agreeableness_nature,
    "Is 'agreeableness' an immutable personality trait (Mountain) or a behavioral set-point that can be recalibrated via operant conditioning (Rope)?",
    resolution_mechanism("Longitudinal study of 'Freds' placed in environments with mandatory boundary enforcement training"),
    impact("If Mountain: Fred is doomed. If Rope: Fred can be trained to use diplomacy."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Diplomatic Enforcement
 * Viability: Fred can solicitously offer to put the requester in touch with 
 * staff or sit on personal requests for a protracted period.
 * Suppression: Often suppressed by the enforcer's own "agreeableness" 
 * and fear of "not being nice".
 * * ALTERNATIVE 2: Explicit Exceptions
 * Viability: Building exceptions into the rule (e.g., "don't email me 
 * unless urgent") so rule-followers can also use the direct path 
 *.
 * * CONCLUSION:
 * The presence of these alternatives shifts the "unintentional" filter into 
 * a "Noose" when the enforcer chooses to identify with their virtue (being nice) 
 * over the functional health of the system.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [asshole_filter_2015].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
