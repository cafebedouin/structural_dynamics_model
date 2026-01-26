% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: the_calm_protocol_suppression
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: the_calm.md (Narrative of Soh and Mbatha)
% ============================================================================

:- module(constraint_the_calm, []).

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
 * * constraint_id: the_calm_protocol_suppression
 * human_readable: The "Calm" of Antarctic Protocol
 * domain: social/psychological/environmental
 * temporal_scope: Modern Antarctic Expedition
 * spatial_scope: Antarctic Corridor / Sled Expedition
 * * SUMMARY:
 * This constraint models the psychological and institutional "weight" described 
 * in the narrative. "The Calm" is a state where the unyielding physical 
 * constraints of the Antarctic (Mountain) are met with a rigid adherence to 
 * "Protocol" (Rope). However, beneath this coordination lies a "weight" 
 * of suppressed trauma—specifically an incident involving a bird, an axe, and 
 * a choice—that functions as a Snare, extracting the mental health of those 
 * who carry the story.
 * * KEY AGENTS:
 * - Soh: Individual powerless/trapped; a senior researcher carrying the weight 
 * of a past failure who eventually transmits it to a junior member 
 *.
 * - Mbatha: Individual powerless; the new host for the story's weight 
 *.
 * - Protocol: Institutional; the set of rules that "doesn't know everything" 
 * but provides the structure for the "calm".
 * * NARRATIVE ARC:
 * The expedition survives the environment by following a Rope (Protocol). 
 * For Soh, however, the silence of the Antarctic is a Snare—it chokes the 
 * ability to speak until the weight "demands transmission." By telling the 
 * story to Mbatha, the weight is not resolved but merely moved, ensuring 
 * the persistence of the psychological constraint.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(the_calm_expedition_window, 0, 10).
narrative_ontology:constraint_claim(the_calm_protocol_suppression, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.75. High extraction; the "weight" extracts the peace and 
% sanity of the host, demanding a transmission that "disturbs" the 
% receiver.
domain_priors:base_extractiveness(the_calm_protocol_suppression, 0.75).

% Suppression score (0.0-1.0)
% Rationale: 0.8. The "Calm" is a state of active suppression of the truth; 
% Soh notes that "Nothing happened" is the standard response to conceal 
% the weight.
domain_priors:suppression_score(the_calm_protocol_suppression, 0.8).

% Enforcement requirements
% Requires active enforcement (expedition hierarchy, field journal 
% recording, and the unspoken rule of "don't hit back").
domain_priors:requires_active_enforcement(the_calm_protocol_suppression).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(the_calm_protocol_suppression, extractiveness, 0.75).
narrative_ontology:constraint_metric(the_calm_protocol_suppression, suppression_requirement, 0.8).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(the_calm_protocol_suppression, short_term_expedition_focus).
constraint_victim(the_calm_protocol_suppression, soh).
constraint_victim(the_calm_protocol_suppression, mbatha).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EXPEDITION CREW - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to establish navigation sequences and nav 
         console monitoring.
   WHEN: immediate - Focused on the 320-kilometer trek and the weather radar.
   WHERE: mobile - Moving between fuel stops and hab-modules.
   SCOPE: local - Immediate corridor survival.
   
   WHY THIS CLASSIFICATION:
   The crew views "Protocol" as a Rope—a functional coordination tool. 
   Without it, they would hit the Mountain of the ice. They believe 
   that following the rules is what keeps them safe.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    the_calm_protocol_suppression,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(the_calm_protocol_suppression, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SOH / MBATHA (LATE) - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Bound by the "weight" and the demand for 
         transmission.
   WHEN: biographical - The story is a weight that stays "forever" unless 
         passed on.
   WHERE: trapped - Bound by the isolation and the presence of the birds.
   SCOPE: local - Personal mental interior.
   
   WHY THIS CLASSIFICATION:
   The story itself is a Snare. It extracts life and peace from the host. 
   The act of telling it is an attempt to "move" the Snare to a new 
   host (Mbatha), but the constraint remains extractive for both 
   participants.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    the_calm_protocol_suppression,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(the_calm_protocol_suppression),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ICE / THE WIND - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the indifference of nature.
   WHEN: historical - Operates outside the human scale of expeditions.
   WHERE: analytical - Not bound by the hab-module or the corridors.
   SCOPE: global - The fundamental physics of the Antarctic.
   
   WHY THIS CLASSIFICATION:
   The environment is a Mountain. It has no interest in protocol or 
   guilt. It simply "knows." It is an unchangeable law that dictates the 
   boundary conditions of the human struggle underneath.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    the_calm_protocol_suppression,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(the_calm_protocol_suppression, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(the_calm_tests).

test(weight_transmission_logic) :-
    % Testing Soh (Snare) vs Crew (Rope)
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, T1, context(individual_powerless, biographical, trapped, local)),
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, T2, context(institutional, immediate, mobile, local)),
    T1 \= T2.

test(suppression_signature) :-
    % The "Calm" requires a high level of active suppression to maintain order.
    domain_priors:suppression_score(the_calm_protocol_suppression, S),
    S >= 0.8.

:- end_tests(the_calm_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. MANDATROPHY ANALYSIS: In 'The Calm', the Mandate is the "Expedition 
 * Progress" (Khatri's push to 5-40). The Margin extracted is the 
 * "Psychological Safety/Honesty" of the crew. By prioritizing the 
 * mission's "Nothing happened" status, the system extracts the engineers' 
 * sanity, turning the story into a terminal Snare.
 * 2. CLASSIFICATION: Labeled as 'Snare' from the individual perspective 
 * because the story acts as a parasite that "demands transmission" to 
 * move its weight.
 * 3. OMEGA: The uncertainty is whether the birds are a physical presence 
 * (Mountain) or a shared hallucination of the weight (Snare).
 */

omega_variable(
    skua_corporeality,
    "Are the skuas a physical manifestation of environmental pressure 
     (Mountain) or a shared psychological index of the 'Weight' (Snare)?",
    resolution_mechanism("Verification of field journal photographs by an 
    external biologist (Analytical Observer)"),
    impact("If physical: They are a Mountain. If shared index: They are 
            part of the psychological Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Institutional Reporting (The "Scaffold")
 * Viability: Formally reporting the incident to end the story's 
 * clandestine life.
 * Suppression: Suppressed by Soh's realization that reporting "would 
 * end the story, and ending the story would mean the weight stays 
 * forever".
 * * ALTERNATIVE 2: Cathartic Confession (The "Alternative Rope")
 * Viability: Talking as a way to "move" the weight rather than suppress it.
 * * CONCLUSION:
 * The choice to transmit the story (Alternative 2) effectively transforms 
 * the Snare into a multi-host organism, ensuring that the "Calm" is 
 * always haunted.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [the_calm_protocol_suppression].
% 2. Analyze: ?- multi_index_report(the_calm_protocol_suppression).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
