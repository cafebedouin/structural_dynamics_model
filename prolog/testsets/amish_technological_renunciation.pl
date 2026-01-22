% ============================================================================
% CONSTRAINT STORY: amish_technological_renunciation
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "What Does It Mean To Be Amish?" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_amish_technological_renunciation, []).

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
 * * constraint_id: amish_technological_renunciation
 * human_readable: The Television Test (Amish Renunciation)
 * domain: social/technological/religious
 * temporal_scope: Contemporary / Perennial
 * spatial_scope: Regional (Amish Country) to Global (The Human Condition)
 * * SUMMARY:
 * This constraint defines "being Amish" as the collective discipline to renounce 
 * technology that is recognized as harmful to the individual and community. 
 * It identifies a gap between the intellectual recognition of harm and the 
 * volitional ability to act upon that recognition.
 * * KEY AGENTS:
 * - Amos (The Amish Man): An agent who embodies the "Rope" of collective renunciation.
 * - The Tourists: Agents who are "trapped" by technology they recognize as negative but cannot quit.
 * - The Social Environment: The "Amish country" versus the "mass culture" represented by the tourists.
 * * NARRATIVE ARC:
 * Tourists seek the meaning of Amish life, expecting a theological answer. 
 * Amos demonstrates that the meaning is actually a constraint: the willingness to 
 * sacrifice individual convenience (television) for community health. 
 * For the tourists, the constraint is a "Noose" of habit; for the Amish, it is 
 * a "Rope" of identity.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% The Structural Anchor
narrative_ontology:interval(amish_technological_renunciation, 0, 10).
narrative_ontology:constraint_claim([amish_technological_renunciation], [technological_governance]).

% Base extractiveness score (0.0-1.0)
% Rationale: High (0.8). For the tourists, mass technology extracts time, community, 
% and agency, as they "believe" it is detrimental but cannot stop. 
% For the Amish, the renunciation extracts convenience to preserve social capital.
domain_priors:base_extractiveness(amish_technological_renunciation, 0.8).

% Suppression score (0.0-1.0)
% Rationale: High (0.75). Mass culture suppresses the "willingness to give up" 
% harmful habits, making the alternative (renunciation) feel impossible for the tourists.
domain_priors:suppression_score(amish_technological_renunciation, 0.75).

% Enforcement requirements
% Requires active enforcement (Ordnung) within the Amish community; 
% emerges naturally as habit/dependency in mass culture.
domain_priors:requires_active_enforcement(amish_technological_renunciation).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(amish_technological_renunciation, community_integrity). % Amish social cohesion
constraint_victim(amish_technological_renunciation, individual_autonomy). % Tourists trapped by habit

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(amish_technological_renunciation, extractiveness, 0.8).
narrative_ontology:constraint_metric(amish_technological_renunciation, suppression_requirement, 0.75).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE TOURIST - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Slaves to the habit of television)
   WHEN: immediate (The present moment of the "Television Test")
   WHERE: trapped (No hands raised to give up the harm)
   SCOPE: global (Mass culture participants)
   
   WHY THIS CLASSIFICATION:
   For the tourists, their relationship with technology is a "Noose." They 
   intellectually identify it as a "negative impact" but are "trapped" by it, 
   liquidating their agency to choose their own values.
   
   NARRATIVE EVIDENCE:
   "How many of you think that television... has a negative impact? ... How many 
   of you are willing to give up television? ... no hands were raised".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    amish_technological_renunciation,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(amish_technological_renunciation, E),
    E > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: AMOS (THE AMISH MEMBER) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "give up television")
   WHEN: biographical (The lived experience of being Amish)
   WHERE: mobile (Can define identity outside mass culture)
   SCOPE: regional (Amish Country)
   
   WHY THIS CLASSIFICATION:
   For the Amish, renunciation is a "Rope"—a functional coordination mechanism. 
   By collectively agreeing to "give up" specific technologies, they coordinate 
   a community that is "more than the individuals," creating a distinct identity.
   
   NARRATIVE EVIDENCE:
   "That is what it means to be Amish".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    amish_technological_renunciation,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The "ancient and wise" observer of human behavior)
   WHEN: civilizational (The fundamental choice between convenience and value)
   WHERE: analytical (Observer stance)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the analyst, the "Television Test" reveals a "Mountain"—an unchangeable 
   law of social physics. The gap between knowing harm and acting to stop it 
   is a zero-degree-of-freedom reality that defines the "Amish" as a 
   specific, rare exception to the general rule of human inertia.
   
   NARRATIVE EVIDENCE:
   "The group looked to one another, but no hands were raised. 'That is what it 
   means to be Amish'".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    amish_technological_renunciation,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(amish_technological_renunciation_tests).

test(multi_perspective_renunciation) :-
    % Tourist (Powerless) sees Noose
    constraint_indexing:constraint_classification(amish_technological_renunciation, noose, context(individual_powerless, immediate, trapped, global)),
    % Amish (Moderate) sees Rope
    constraint_indexing:constraint_classification(amish_technological_renunciation, rope, context(individual_moderate, biographical, mobile, regional)),
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(amish_technological_renunciation, mountain, context(analytical, civilizational, analytical, global)),
    Type1 \= Type2, Type2 \= Type3.

test(power_extractiveness_renunciation) :-
    % The powerless tourist experiences the "Noose" of habit-extraction.
    ContextPowerless = context(individual_powerless, immediate, trapped, global),
    ContextModerate = context(individual_moderate, biographical, mobile, regional),
    constraint_indexing:extractiveness_for_agent(amish_technological_renunciation, ContextPowerless, E1),
    constraint_indexing:extractiveness_for_agent(amish_technological_renunciation, ContextModerate, E2),
    E1 > E2.

test(habit_immutability) :-
    % In the immediate horizon, the inability to quit is a Mountain (fact).
    constraint_indexing:effective_immutability(immediate, trapped, mountain).

:- end_tests(amish_technological_renunciation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.8):
 * Reasoning: I chose high extraction because the text highlights that the tourists 
 * *know* the technology is a "negative impact" yet they are "liquidated" of the 
 * power to change. The technology extracts their "community" and "life".
 * * 2. PERSPECTIVE SELECTION:
 * Contrasted the "trapped" Tourists (Noose) with the "mobile" Amish (Rope) to 
 * show the core pedagogical point: Amish life is a "Rope" of discipline that 
 * avoids the "Noose" of uncritical consumption.
 * * 3. MANDATROPHY RESOLUTION:
 * Status: [RESOLVED MANDATROPHY]. High extraction of convenience (Amish) or 
 * attention (Tourists) is resolved by the "Amish Test," which identifies 
 * renunciation as a functional tool (Rope) for the preservation of value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_agency_threshold,
    "Is the 'willingness to give up' television a biological limit (Mountain) 
     or a learnable social skill (Rope)?",
    resolution_mechanism("Audit of exit rates from high-tech habits in individuals moving to intentional communities"),
    impact("If limit: Mass culture is a terminal Noose. If skill: Amish life is a teachable Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: "Better Programming" / Reform
 * Viability: The idea that we can keep technology but fix the content.
 * Suppression: Shunted by the blog author: "the problems of society will not 
 * be solved by advocating for better programming".
 * * ALTERNATIVE 2: Pure Theological Belief (Jesus)
 * Viability: Being religious without changing lifestyle.
 * Suppression: Explicitly identified as "not what it means" to be Amish by 
 * Amos.
 * * CONCLUSION:
 * The absence of these alternatives (Reform/Pure Belief) makes the Amish 
 * "Television Test" the definitive "Rope" for survival.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [amish_technological_renunciation].
% Multi-perspective: ?- constraint_indexing:multi_index_report(amish_technological_renunciation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
