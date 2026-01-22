% ============================================================================
% CONSTRAINT STORY: theory_of_visitors
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Sam Lansky / cafebedouin.org, "The Theory of Visitors"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_theory_of_visitors, []).

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
 * * constraint_id: theory_of_visitors
 * human_readable: The Theory of Visitors (Relationship Transience)
 * domain: social/psychological
 * temporal_scope: Perennial / Biographical
 * spatial_scope: Global / Interpersonal
 * * SUMMARY:
 * This constraint defines all human relationships as inherently transient "visitors" who 
 * arrive for a limited time and inevitably depart. It posits a 
 * fundamental trade-off: visitors take something irreplaceable from the agent 
 * upon departure, but leave "souvenirs" (memories/lessons) that can be kept forever.
 * * KEY AGENTS:
 * - The Host (The Individual): The agent who must navigate the "arrival" and 
 * "surrender" to the "departure" of others.
 * - The Visitor: Friends, relatives, or lovers who inhabit the agent's life 
 * temporarily.
 * - The Digital Persona: The modern "swipe-right" iteration of the visitor 
 * that encourages quick judgment and transient interaction.
 * * NARRATIVE ARC:
 * An agent is confronted with the "secret" that everything is temporary. 
 * They must choose between the "Noose" of grieving for what is taken or 
 * using the theory as a "Rope" to welcome and surrender to the flux of life. 
 * In the digital age, this transience is amplified into a "Standard of Doing" 
 * that can prevent deep interaction.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(theory_of_visitors, 0, 10).
narrative_ontology:constraint_claim([theory_of_visitors], [ontological_transience]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Moderate-High (0.65). Visitors "take something from you... that 
% you can’t ever get back," liquidating a portion of the host's being.
domain_priors:base_extractiveness(theory_of_visitors, 0.65).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.5). The "Theory of Visitors" suppresses the illusion 
% of permanence and the possibility of "forever" relationships to encourage 
% "surrender".
domain_priors:suppression_score(theory_of_visitors, 0.5).

% Enforcement requirements
% Emerges naturally from the human condition of mortality and social flux.
domain_priors:emerges_naturally(theory_of_visitors).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(theory_of_visitors, extractiveness, 0.65).
narrative_ontology:constraint_metric(theory_of_visitors, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(theory_of_visitors, emotional_resilience). % Those who "surrender".
constraint_victim(theory_of_visitors, relational_permanence).    % The desire for things to "stay".

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MOURNER / THE BEREFT - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Target of the "part that sucks")
   WHEN: immediate (The moment of "departure" or being "stabbed in the back")
   WHERE: trapped (Bounded by the loss of "something that you can't ever get back")
   SCOPE: local (The personal void left by a visitor)
   
   WHY THIS CLASSIFICATION:
   For the individual experiencing loss, transience is a "Noose." The visitor 
   extracts an irreplaceable part of the self and leaves, strangling the 
   host's sense of continuity and meaning.
   
   NARRATIVE EVIDENCE:
   "when the visitors go home, they might take something from you. Something 
   that you can’t ever get back. And that part sucks".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    theory_of_visitors,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(theory_of_visitors, E),
    E > 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STOIC / THE NAVIGATOR - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "welcome" and "surrender")
   WHEN: biographical (The lifespan of "souvenirs" kept forever)
   WHERE: mobile (Moving from one "visitor" to the next)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the self-aware host, the theory is a "Rope"—a functional coordination 
   mechanism for internal peace. It provides a map for "surrender" that 
   prevents total liquidation by emphasizing the "souvenirs" over the 
   losses.
   
   NARRATIVE EVIDENCE:
   "you welcome their arrival, and you surrender to their departure... But 
   visitors always leave souvenirs. And you get to keep those forever".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    theory_of_visitors,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The observer of "the theory of visitors")
   WHEN: civilizational (The universal truth of transience)
   WHERE: analytical (Observer stance)
   SCOPE: global (Applies to all relationships)
   
   WHY THIS CLASSIFICATION:
   To the analyst, transience is a "Mountain"—an unchangeable natural law of 
   the human social field. It is a zero-degree-of-freedom reality: humans are 
   mortal and relationships are finite.
   
   NARRATIVE EVIDENCE:
   "All relationships are transient... Everything is temporary. People come 
   into your life... and then they go away".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    theory_of_visitors,
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

:- begin_tests(theory_of_visitors_tests).

test(multi_perspective_visitors) :-
    % Mourner sees Noose
    constraint_indexing:constraint_classification(theory_of_visitors, noose, context(individual_powerless, immediate, trapped, local)),
    % Stoic sees Rope
    constraint_indexing:constraint_classification(theory_of_visitors, rope, context(individual_moderate, biographical, mobile, local)),
    % Observer sees Mountain
    constraint_indexing:constraint_classification(theory_of_visitors, mountain, context(analytical, civilizational, analytical, global)),
    Type1 \= Type2, Type2 \= Type3.

test(power_extractiveness_visitors) :-
    % The powerless victim (trapped in loss) experiences higher extraction 
    % than the moderate navigator who keeps "souvenirs".
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, local),
    constraint_indexing:extractiveness_for_agent(theory_of_visitors, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(theory_of_visitors, ContextModerate, Score2),
    Score1 > Score2.

test(time_immutability_visitors) :-
    % In the immediate horizon, departure is an unchangeable Mountain (fact).
    constraint_indexing:effective_immutability(immediate, trapped, mountain).

:- end_tests(theory_of_visitors_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.65):
 * Reasoning: I chose a moderate-high score because the text identifies a 
 * "part that sucks"—the permanent loss of something irreplaceable. 
 * This is a high-asymmetry event where the individual loses part of their 
 * biographical "capital" to the flux of the universe.
 * * 2. PERSPECTIVE SELECTION:
 * Contrasted the individual suffering from "stabbed in the back" or "relatives 
 * who die" (Noose) with the practitioner who "welcomes" and "surrenders" 
 * (Rope). The Analytical stance (Mountain) captures the text's assertion of 
 * universal transience.
 * * 3. STATUS: [RESOLVED MANDATROPHY]
 * Reasoning: The high extraction (loss of something irreplaceable) is 
 * "resolved" by the introduction of "souvenirs" and the agency of 
 * "surrender," transforming the extraction into a tool for emotional growth (Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    souvenir_valuation_omega,
    "Is the value of the 'souvenir' truly enough to offset the 'part that sucks' (Rope), 
     or is the theory a narrative coping mechanism for a terminal Noose?",
    resolution_mechanism("Longitudinal tracking of long-term well-being in 'Stoics' vs. 'Mourners'"),
    impact("If souvenirs offset loss: Theory is a Rope. If loss is terminal: Theory is a Noose."),
    confidence_without_resolution(medium)
).

omega_variable(
    digital_transience_causality,
    "Does 'swipe-right' culture create the 'Visitor' state (Noose), or merely 
     reveal the existing 'Mountain' of human transience?",
    resolution_mechanism("Comparison of relationship duration in 'high-tech' vs 'low-tech' isolated communities"),
    impact("If culture creates it: Standard of Thinking (Noose). If it reveals it: Analytical tool (Mountain)."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Eternal Commitment / "Forever" Narrative
 * Viability: The traditional social "Standard of Thinking" for marriage and loyalty.
 * Suppression: Rejected by the "Theory of Visitors" as an illusion; "Everything 
 * is temporary".
 * * ALTERNATIVE 2: Deep Interaction / Time Investment
 * Viability: Irrespective of duration, investing time in a complicated person.
 * Suppression: Suppressed by "quick judgments" and "swipe-right" culture.
 * * CONCLUSION:
 * The existence of Alternative 2 suggests that while transience may be a 
 * Mountain, the *depth* of interaction is a Rope that can be used to 
 * mitigate the extraction.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [theory_of_visitors].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(theory_of_visitors).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
