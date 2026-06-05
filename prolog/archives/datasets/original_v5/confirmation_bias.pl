% ============================================================================
% CONSTRAINT STORY: confirmation_bias
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Cognitive Psychology / Wason (1960)
% ============================================================================

:- module(constraint_confirmation_bias, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: confirmation_bias
 * human_readable: Confirmation Bias
 * domain: social/cognitive/political
 * temporal_scope: Permanent (Human Evolutionary History)
 * spatial_scope: Global (Human Cognition)
 * * SUMMARY:
 * Confirmation bias is the tendency to search for, interpret, favor, and recall 
 * information in a way that confirms one's prior beliefs or values. It acts 
 * as a structural filter on reality, preventing the objective processing 
 * of contradictory evidence.
 * * KEY AGENTS:
 * - The Believer: An agent seeking validation for an existing world-view.
 * - The Dissenter: An agent presenting contradictory evidence that is ignored.
 * - The Algorithm: A digital system that automates and scales the bias through 
 * echo chambers.
 * * NARRATIVE ARC:
 * The bias functions as a "Mountain" of biological pre-programming. For the 
 * individual, it is a "Rope" providing cognitive ease and social cohesion. 
 * In polarized societies, it becomes a "Snare," as groups become trapped in 
 * incompatible realities, strangling the possibility of collective 
 * problem-solving.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(confirmation_bias_interval, 0, 10).
narrative_ontology:constraint_claim(confirmation_bias, snare).

% Base extractiveness: 0.3 (Moderate)
% Rationale: It extracts "objective truth" and "rationality." It benefits 
% the ego and existing power structures by ignoring the costs of incorrect beliefs.
domain_priors:base_extractiveness(confirmation_bias, 0.3).

% Suppression: 0.8 (High)
% Rationale: The bias inherently suppresses contradictory information by 
% making it invisible or devaluing it before it can be cognitively processed.
domain_priors:suppression_score(confirmation_bias, 0.8).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(confirmation_bias, extractiveness, 0.3).
narrative_ontology:constraint_metric(confirmation_bias, suppression_requirement, 0.8).

% Enforcement: Emerges naturally from neural efficiency and identity protection.
domain_priors:emerges_naturally(confirmation_bias).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(confirmation_bias, [ideological_leaders, status_quo]).
constraint_victim(confirmation_bias, [scientific_objectivity, minority_dissenters]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NEUROSCIENTIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The objective observer)
   WHEN: civilizational (Observing a universal biological trait)
   WHERE: trapped (Human brain architecture is currently fixed)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the scientist, confirmation bias is a Mountain. It is an evolved 
   efficiency mechanism—the brain's way of conserving energy by not 
   re-evaluating every belief. It is an unchangeable feature of the 
   human hardware.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    confirmation_bias,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COMMUNITY LEADER - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (Rule-shaping power within a group)
   WHEN: biographical (Maintaining social order over decades)
   WHERE: arbitrage (Can frame narratives to align with group bias)
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For the leader, the bias is a Rope. It is a coordination mechanism that 
   builds social capital and group identity. By reinforcing shared beliefs, 
   the leader pulls the community together, creating a unified force that is 
   resistant to external disruption.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    confirmation_bias,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE POLARIZED CITIZEN - SNARE
   --------------------------------------------------------------------------
   
   WHO: powerless (A subject in a high-conflict social system)
   WHEN: immediate (Today's news cycle and social interaction)
   WHERE: constrained (Cannot easily leave their information bubble)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the citizen trapped in a polarized environment, the bias is a Snare. 
   Every piece of information they consume tightens their existing 
   prejudices, making it impossible to see the "other side" as human or 
   rational. Their ability to think independently is strangled by the 
   echo chamber they inhabit.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    confirmation_bias,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(confirmation_bias_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(confirmation_bias, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(confirmation_bias, T2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(confirmation_bias, T3, context(agent_power(powerless), _, _, _)),
    T1 \= T2, T2 \= T3.

test(bias_suppression_scaling) :-
    % Confirmation bias relies on high internal suppression of alternatives.
    domain_priors:suppression_score(confirmation_bias, Score),
    Score >= 0.7.

test(time_horizon_immutability) :-
    % Civilizational view sees biological invariants as Mountains.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(confirmation_bias_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION SCORE (0.8): The bias is defined by the active avoidance 
 * and devaluing of contradictory evidence.
 * 2. SNARE CLASSIFICATION: I chose this for the citizen because the 
 * "echo chamber" effect is an active trap that consumes the agent's 
 * epistemic agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_feedback_weight,
    "To what extent is modern polarization caused by biological bias (Mountain) vs. algorithmic amplification (Snare)?",
    resolution_mechanism("Comparative study of polarized groups with and without digital social media exposure"),
    impact("If Mountain: Education is the only fix. If Snare: Regulation can loosen the trap."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Falsificationism (Scientific Method)
 * Viability: Actively seeking to disprove one's beliefs.
 * Suppression: High. It is cognitively expensive and socially isolating.
 * * CONCLUSION:
 * The existence of the scientific method as an alternative makes 
 * confirmation bias a Snare for those who refuse (or are unable) to 
 * use it, but a Rope for those who use it strategically.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_confirmation_bias].
 * 2. Multi-perspective: ?- multi_index_report(confirmation_bias).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
