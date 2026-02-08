% ============================================================================
% CONSTRAINT STORY: belief_argument_conclusion
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Iain M. Banks, "Use of Weapons" (via cafebedouin.org)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_belief_argument_conclusion, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Updated for v3.4) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: belief_argument_conclusion
 * human_readable: The Instinctive Belief Constraint
 * domain: social/philosophical
 * temporal_scope: Ongoing / Human Nature
 * spatial_scope: Cognitive / Interpersonal
 * * SUMMARY:
 * This constraint represents the "futile" nature of argument as a tool for 
 * changing belief. It posits that beliefs are instinctive 
 * and justifications are secondary "excuses". When 
 * coordination via "agreeing to disagree" fails, the only remaining 
 * conclusion is "to fight".
 * * KEY AGENTS:
 * - The Professor (Analytical Observer): Views argument as a performance 
 * and belief as an immutable biological/instinctive fact.
 * - Erens (The Interlocutor): Represents the common belief in the utility 
 * of argument and negotiation.
 * - The Arguer (Individual Powerless): Subject to their own instinctive 
 * justifications and the ego-trap of debate.
 * * NARRATIVE ARC:
 * The dialogue deconstructs the "Rope" of argument, revealing it as a 
 * "Snare" that consumes energy without changing the underlying "Mountain" 
 * of instinctive belief. This leads to a binary 
 * conclusion: passive disagreement or active coercion (fighting).
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (REVISED)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(belief_argument_conclusion, 0, 10).
narrative_ontology:constraint_claim(belief_argument_conclusion, snare).

% Base Properties
% Rationale: 0.70 extraction of cognitive labor ("excuses").
domain_priors:base_extractiveness(belief_argument_conclusion, 0.70).
domain_priors:suppression_score(belief_argument_conclusion, 0.80).
domain_priors:theater_ratio(belief_argument_conclusion, 0.80). % Reflects performative justifications.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(belief_argument_conclusion, extractiveness, 0.7).
narrative_ontology:constraint_metric(belief_argument_conclusion, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(belief_argument_conclusion, theater_ratio, 0.8).

% Mandatory keys for classification engine v3.4
% Resolved MISSING_TEMPORAL_DATA by anchoring metrics for drift detection.
% Emerges naturally from human psychology.
domain_priors:emerges_naturally(belief_argument_conclusion).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(belief_argument_conclusion, instinctive_certainty).
narrative_ontology:constraint_victim(belief_argument_conclusion, the_arguer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PROFESSOR (ANALYTICAL) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observing the "futile" machinery of debate)
   WHEN: civilizational (Fundamental human behavioral patterns)
   WHERE: analytical (Unconstrained by the need to win)
   SCOPE: global (Universal human trait)
   
   WHY THIS CLASSIFICATION:
   The Professor sees instinctive belief as a "Mountain"—a fixed biological 
   reality that justifications cannot touch. Argument is 
   not a tool for change but an unchangeable feature of ego-maintenance 
  .
   
   NARRATIVE EVIDENCE:
   "I strongly suspect the things people believe in are usually just what 
   they instinctively feel is right... you’ve attacked the wrong thing" 
  .
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    belief_argument_conclusion,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ARGUER (INDIVIDUAL POWERLESS) - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Subject to their own instinct and social anger)
   WHEN: immediate (Caught in a specific debate/argument)
   WHERE: trapped (Bounded by the inability to have their mind changed)
   SCOPE: local (Immediate interpersonal conflict)
   
   WHY THIS CLASSIFICATION:
   For the participant, argument is a "Snare." It tightens as they "trot out 
   their excuses," consuming their energy and triggering anger when they 
   realize they cannot reach the other's instinct.
   
   NARRATIVE EVIDENCE:
   "one of the reasons that people become angry when they argue is that 
   they realize... [other people are not prepared to have their minds changed]" 
  .
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    belief_argument_conclusion,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE NEGOTIATOR (INSTITUTIONAL) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (Seeking a conclusion to a conflict)
   WHEN: biographical (Hiring/Diplomatic/Workplace timelines)
   WHERE: mobile (Alternatives like 'fighting' are visible but avoided)
   SCOPE: regional/national
   
   WHY THIS CLASSIFICATION:
   For an institution, negotiation is a "Rope"—a functional mechanism 
   to coordinate a conclusion, even if the underlying beliefs remain 
   untouched. It is a tool for systemic stability.
   
   NARRATIVE EVIDENCE:
   "Negotiation is a way to come to a conclusion; it’s the type of 
   conclusion I’m talking about".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    belief_argument_conclusion,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(belief_argument_conclusion_tests).

test(multi_perspective_belief) :-
    constraint_indexing:constraint_classification(belief_argument_conclusion, T1, context(analytical, _, _, _)),
    constraint_indexing:constraint_classification(belief_argument_conclusion, T2, context(powerless, _, _, _)),
    constraint_indexing:constraint_classification(belief_argument_conclusion, T3, context(institutional, _, _, _)),
    T1 \= T2, T2 \= T3.

test(power_extractiveness_belief) :-
    ContextPowerless = context(powerless, immediate, trapped, local),
    ContextInstitutional = context(institutional, biographical, mobile, regional),
    constraint_indexing:extractiveness_for_agent(belief_argument_conclusion, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(belief_argument_conclusion, ContextInstitutional, Score2),
    Score1 > Score2. % The powerless arguer loses more (ego/energy)

test(belief_immutability) :-
    % Instinctive belief is unchangeable across horizons
    constraint_indexing:effective_immutability(immediate, trapped, mountain),
    constraint_indexing:effective_immutability(civilizational, analytical, mountain).

:- end_tests(belief_argument_conclusion_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.7):
 * Reasoning: Wasted labor on "excuses" is a high extraction from the 
 * subject's cognitive budget. The binary of "fight" as the alternative 
 * to disagreement indicates severe asymmetric costs in the social domain.
 * * 2. SUPPRESSION SCORE (0.8):
 * Reasoning: The text suggests that the real core of belief is invisible 
 * to the conscious arguer. Alternatives like "changing the mind" are 
 * presented as illusions.
 * * 3. PERSPECTIVE SELECTION:
 * Chose the Professor (Analyst), the Arguer (Powerless), and the Negotiator 
 * (Institutional) to show the gap between the perceived tool (Rope) and 
 * the actual psychological trap (Snare/Mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instinct_vs_intellect,
    "Are beliefs truly 100% instinctive, or can 'attacking the justifications' 
     eventually erode the core belief (Scaffold)?",
    resolution_mechanism("Long-term cognitive science study on belief-erosion via justification-denial"),
    impact("If justifications matter: Argument is a Rope. If only instinct matters: It is a permanent Mountain."),
    confidence_without_resolution(medium)
).

omega_variable(
    belief_argument_conclusion_extraction_intent,
    "Is the futility of argument a biological necessity (Mountain) or an 
     intentional ego-strategy to maintain social dominance (Snare)?",
    resolution_mechanism("Comparison of conflict outcomes in environments with vs. without 'high-rationality' training"),
    impact("If necessity: Mountain. If strategy: Mandatrophy Snare."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Socratic Method (Iterative Questioning)
 * Viability: Aims to find the "instinct" rather than the "excuse".
 * Suppression: Ignored by the Professor's binary of "Agree or Fight".
 * * ALTERNATIVE 2: Empathy-Based Resolution
 * Viability: Addresses the "wrong thing" (feelings) directly.
 * Suppression: Punished/Hidden by the social tendency to overvalue 
 * "argument" and "excuses".
 * * CONCLUSION:
 * The text's conclusion that only "Agree to Disagree or Fight" remains 
 * suggests a highly suppressive environment where the "Rope" of genuine 
 * belief-transformation has been removed.
 */

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional inquiry (0.20) toward the 
% performative "Theater of Excuses" (0.80) as the argument reaches impasse.
narrative_ontology:measurement(belief_tr_t0, belief_argument_conclusion, theater_ratio, 0, 0.20).
narrative_ontology:measurement(belief_tr_t5, belief_argument_conclusion, theater_ratio, 5, 0.55).
narrative_ontology:measurement(belief_tr_t10, belief_argument_conclusion, theater_ratio, 10, 0.80).

% Extraction: Tracking the intensification of cognitive labor extraction 
% as the subject moves from discussion to the binary of "agree or fight."
narrative_ontology:measurement(belief_ex_t0, belief_argument_conclusion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(belief_ex_t5, belief_argument_conclusion, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(belief_ex_t10, belief_argument_conclusion, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraint_belief_argument_conclusion].

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
