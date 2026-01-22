% ============================================================================
% CONSTRAINT STORY: toxic_social_infection
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Muir, Shultz, Martin, wikiHow, Sandalis, Glaser, Narcissist’s Prayer
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_toxic_social_infection, []).

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
 * * constraint_id: toxic_social_infection
 * human_readable: The Infecting Character (Hoodoo/Vampire)
 * domain: social/psychological
 * temporal_scope: Perennial / Biographical
 * spatial_scope: Local (Newsrooms, families, waterfront communities)
 * * SUMMARY:
 * This constraint describes a class of individuals—referred to variously as "hoodoos," "psychic vampires," 
 * "landmine people," or "infecting characters"—who radiate an inward instability that draws disaster 
 * upon themselves and those around them. They function as "sucking black holes" that 
 * extract energy, time, and agency from their social environment.
 * * KEY AGENTS:
 * - The Infector (Hoodoo/Vampire): One who creates a "whirling vortex of negativity" and drains others.
 * - The Victim (The Infected): The agent who becomes "poisoned" or "enmeshed" by attempting to help or engage.
 * - The Analyst (The Gestalt Observer): One who uses the energy test to distinguish toxic from nourishing relationships.
 * * NARRATIVE ARC:
 * The interaction begins with a "seductive" aura or "amiable" flattery. Once enmeshed, 
 * the subject experiences a "drain" of time and energy, potentially becoming an infector 
 * themselves ("FLEAs" or "new recruits"). The only successful resolution is 
 * radical "fleeing" and the setting of firm boundaries.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(toxic_social_infection_interval, 0, 10).
narrative_ontology:constraint_claim([toxic_social_infection], [psychological_hazard]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.9). These agents "suck the life and energy out of you" and "drain time and energy". 
% They are "vampires" who leave the subject "tired" or "poisoned".
domain_priors:base_extractiveness(toxic_social_infection, 0.9).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate-High (0.7). The "amiable friend" facade and "dazzling trappings of success" 
% suppress the underlying disaster. The "Narcissist’s Prayer" actively suppresses truth 
% through gaslighting.
domain_priors:suppression_score(toxic_social_infection, 0.7).

% Enforcement: Emerges naturally from "inward instability" but requires active maintenance 
% of boundaries to resist.
domain_priors:requires_active_enforcement(toxic_social_infection).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(toxic_social_infection, extractiveness, 0.9).
narrative_ontology:constraint_metric(toxic_social_infection, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(toxic_social_infection, the_infector). % Gains time, energy, and capital
constraint_victim(toxic_social_infection, the_infected).      % Loses energy, health, and agency

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE INFECTED VICTIM - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A "little person" who becomes "enmeshed" or a "new recruit")
   WHEN: immediate (Short-term exhaustion after a drink or dinner)
   WHERE: trapped (Newsrooms or family circles where disaster always ensues)
   SCOPE: local (The "silent epidemic" in today’s families)
   
   WHY THIS CLASSIFICATION:
   For the victim, the infector is a "Noose." Engagement—even well-intentioned help—tightens the 
   connection, sucking away life and energy until the subject is "sitting in the corner... 
   spitting bile".
   
   NARRATIVE EVIDENCE:
   "Avoid them at all costs... once they suck you into their dark world, you become 
   one, too".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    toxic_social_infection,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(toxic_social_infection, E),
    E > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE NARCISSIST (INFECTOR) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-making power over the relationship's narrative)
   WHEN: biographical (Long-term "string of failures" or career in a newsroom)
   WHERE: mobile (They "walk away" after rigging the universe to explode)
   SCOPE: local/regional
   
   WHY THIS CLASSIFICATION:
   For the infector, the social environment is a "Rope"—a tool for "self-serving" coordination 
   to gain capital or attention. They use "flattery" and "justifications" to 
   pull themselves out of disaster while letting others suffer the consequences.
   
   NARRATIVE EVIDENCE:
   "They are usually highly self-interested and lack empathy. The relationships they 
   form are largely self serving".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    toxic_social_infection,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GESTALT OBSERVER (SAGE) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The "ancient and wise" observer of "pathology")
   WHEN: historical (The "lesson" remains constant for the rest of your life)
   WHERE: analytical (Observer stance; "Stand back. Be warned")
   SCOPE: global (The "way of the world")
   
   WHY THIS CLASSIFICATION:
   To the analyst, toxicity is a "Mountain"—an immutable law of social physics. The energy 
   test is "almost infallible," and the outcome of engagement is a fixed biological 
   "poisoning" that cannot be argued with.
   
   NARRATIVE EVIDENCE:
   "The test is almost infallible... You have spent some time... at the end... you 
   observe whether you are more energized or less energized".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    toxic_social_infection,
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

:- begin_tests(toxic_social_infection_tests).

test(multi_perspective_vortex) :-
    % Victim sees Noose (Liquidation)
    constraint_indexing:constraint_classification(toxic_social_infection, noose, context(individual_powerless, immediate, trapped, local)),
    % Narcissist sees Rope (Utility)
    constraint_indexing:constraint_classification(toxic_social_infection, rope, context(institutional, biographical, mobile, local)),
    % Sage sees Mountain (Law of Nature)
    constraint_indexing:constraint_classification(toxic_social_infection, mountain, context(analytical, historical, analytical, global)),
    Type1 \= Type2,
    Type2 \= Type3.

test(energy_extraction_scaling) :-
    % The powerless victim experiences the full 0.9 extraction of vitality.
    constraint_indexing:extractiveness_for_agent(toxic_social_infection, context(individual_powerless, immediate, trapped, local), E1),
    constraint_indexing:extractiveness_for_agent(toxic_social_infection, context(institutional, biographical, mobile, local), E2),
    E1 > E2.

test(time_immutability_infection) :-
    % Short-term enmeshment feels like a changeable mistake (Rope/Noose).
    % Long-term pathology is an unchangeable social fact (Mountain).
    constraint_indexing:effective_immutability(historical, analytical, mountain).

:- end_tests(toxic_social_infection_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.9):
 * Reasoning: The text uses terms like "vampire," "poisoned," "drain," and "sucking the life 
 * out." This represents near-total extraction of the victim's social and psychological 
 * resources.
 * * 2. MANDATROPHY STATUS:
 * Status: [RESOLVED MANDATROPHY]. The constraint is a lethal Noose for the empathic subject 
 * but functions as a Rope for the narcissistic beneficiary who rigs the world to 
 * "explode" while they "walk away".
 * * 3. PERSPECTIVE SELECTION:
 * Chose the Victim (Noose), the Narcissist (Rope), and the Gestalt Sage (Mountain) 
 * to capture the range from suffering to predatory utility to detached analysis.
 * * 4. AMBIGUITIES:
 * The source says it is "not necessarily true" that one person is toxic in every 
 * relationship. This is resolved by the energy test being context-dependent 
 * yet "almost infallible" for that specific dyad.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    toxic_social_infection_extraction_intent,
    "Is the 'inward instability' of the infector a functional biological necessity (Mountain) or an intentional strategy for wealth/status extraction (Noose)?",
    resolution_mechanism("Audit of Cluster B neurological empathy-response vs. long-term profit-seeking behaviors"),
    impact("If necessity: Genetic Mountain. If predatory choice: Mandatrophy Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    flea_transmission_rate,
    "Do 'FLEAs' permanently transform the subject into a new infector (Mountain), or are they temporary habits that can be unlearned (Rope)?",
    resolution_mechanism("Longitudinal tracking of behavior in individuals post-estrangement from narcissists"),
    impact("If permanent: Social Mountain of corruption. If temporary: Remediable Rope."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Reform/Therapy
 * Viability: The instinct to "try and help".
 * Suppression: Explicitly suppressed/punished as a "waste of time" that leads to 
 * becoming "enmeshed".
 * * ALTERNATIVE 2: Nourishing Relationships
 * Viability: The Gestalt "whole" where people energize one another.
 * Suppression: Often shunted by the "seductive" aura of the vampire or the 
 * "silent epidemic" of unacknowledged family strain.
 * * CONCLUSION:
 * The presence of Alternative 1 (Help) is identified as the "wrong thing" to attack. 
 * Its suppression by the Sage shifts the toxic interaction from a potentially 
 * repairable Rope into a definitive "Noose" that must be cut.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [toxic_social_infection].
% Multi-perspective: ?- constraint_indexing:multi_index_report(toxic_social_infection).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
