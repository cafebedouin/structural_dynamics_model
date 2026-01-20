% ============================================================================
% CONSTRAINT STORY: dunning_kruger_effect
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Dunning, D., & Kruger, J. (1999) / Social Psychology
% ============================================================================

:- module(constraint_dunning_kruger_effect, []).

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
 * * constraint_id: dunning_kruger_effect
 * human_readable: Dunning-Kruger Effect
 * domain: social/cognitive/educational
 * temporal_scope: Permanent (Cognitive Architecture)
 * spatial_scope: Global (Human Decision Making)
 * * SUMMARY:
 * The Dunning-Kruger effect is a cognitive bias where people with limited 
 * competence in a domain overestimate their own ability. This occurs because 
 * the skills required for competence are the same skills needed to recognize 
 * incompetence. Conversely, high-performers often underestimate their 
 * relative superiority, assuming others find tasks as easy as they do.
 * * KEY AGENTS:
 * - The Novice: Overconfident due to a lack of "meta-cognitive" awareness.
 * - The Expert: Competent but prone to "false consensus" regarding others' ability.
 * - The Scientific Observer: Mapping the non-linear relationship between 
 * actual skill and perceived skill.
 * * NARRATIVE ARC:
 * The effect functions as a "Mountain" of biological limitation in self-assessment. 
 * For the novice, it is a "Rope" (unwarranted confidence) that allows them 
 * to take risks and start learning. However, it becomes a "Noose" in 
 * high-stakes environments where "Peak Stupidity" leads to catastrophic 
 * errors that the agent cannot foresee.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(dunning_kruger_interval, 0, 10).
narrative_ontology:constraint_claim(dunning_kruger_effect, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: It extracts "correctness" and "safety." Overconfidence in 
% incompetent actors extracts resources from the collective (errors) while 
% the actor extracts social status they haven't earned.
domain_priors:base_extractiveness(dunning_kruger_effect, 0.3).

% Suppression score (0.0-1.0)
% Rationale: The bias itself suppresses the visibility of the "competence" 
% alternative. You cannot see the alternative (being correct) because you 
% lack the tools to recognize your own error.
domain_priors:suppression_score(dunning_kruger_effect, 0.7).

% Enforcement: Emerges naturally from human cognitive architecture.
domain_priors:emerges_naturally(dunning_kruger_effect).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(dunning_kruger_effect, extractiveness, 0.3).
narrative_ontology:constraint_metric(dunning_kruger_effect, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(dunning_kruger_effect, overconfident_novices).
constraint_victim(dunning_kruger_effect, [expert_mentors, safety_critical_systems]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   -------------------------------------------------------------------------- */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RESEARCH PSYCHOLOGIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The objective observer)
   WHEN: civilizational (Observing a universal cognitive invariant)
   WHERE: trapped (Human brain structure is currently fixed)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the researcher, the effect is a Mountain. It is a predictable, 
   cross-cultural feature of human information processing. It is not a 
   choice or a social construct, but a physical limit on the precision 
   of self-reflective feedback loops.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dunning_kruger_effect,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STARTUP FOUNDER - ROPE
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (An agent with strategic goals)
   WHEN: biographical (Achieving success through high-risk ventures)
   WHERE: mobile (Can learn and adjust overconfidence over time)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the entrepreneur, a degree of Dunning-Kruger is a Rope. It provides 
   the "delusional" optimism required to coordinate resources and start a 
   project that an expert might deem too difficult. This unwarranted 
   confidence acts as a tether to a future reality they are trying to build.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dunning_kruger_effect,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PASSENGER/VICTIM - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A person subject to someone else's 'expertise')
   WHEN: immediate (A crisis requiring actual skill)
   WHERE: trapped (Cannot escape the incompetent actor's decisions)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For someone whose safety depends on a novice who *thinks* they are an expert 
   (e.g., a passenger with an overconfident, untrained pilot), the effect is 
   a Noose. The novice's lack of meta-cognition strangles the victim's 
   safety, and the victim is trapped by the novice's inability to recognize 
   their own fatal limitations.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dunning_kruger_effect,
    noose,
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

:- begin_tests(dunning_kruger_effect_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(dunning_kruger_effect, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(dunning_kruger_effect, T2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(dunning_kruger_effect, T3, context(agent_power(individual_powerless), _, _, _)),
    T1 \= T2, T2 \= T3.

test(meta_cognitive_extraction) :-
    % Novices extract unearned status/confidence; Mentors pay the cost of errors.
    domain_priors:base_extractiveness(dunning_kruger_effect, Score),
    Score >= 0.3.

test(skill_acquisition_exit) :-
    % Biographical view allows for learning (exit/mobile), converting Noose to Rope.
    constraint_indexing:constraint_classification(dunning_kruger_effect, rope, context(_, biographical, mobile, _)).

:- end_tests(dunning_kruger_effect_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS MADE BY MODEL:
 * * 1. SUPPRESSION SCORE (0.7):
 * The core of the effect is the *inability* to see the alternative 
 * (incompetence). This is a high internal suppression score—not enforced 
 * by a state, but by the agent's own cognitive blindness.
 * * 2. PERSPECTIVE SELECTION:
 * The Analyst sees the Graph (Mountain).
 * The Individual Novice sees the Ladder (Rope).
 * The Witness to the Crash sees the Trap (Noose).
 * * 3. AMBIGUITIES:
 * There is some debate in modern psychology about whether the effect is 
 * as strong as originally claimed (statistical noise vs. real bias). I 
 * treated it as a real biological constraint for this analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statistical_regression_artifact,
    "Is the Dunning-Kruger effect a fundamental cognitive bias or a mathematical artifact of regression toward the mean?",
    resolution_mechanism("Replication of studies using non-traditional noise-correction models"),
    impact("If Artifact: The Mountain is an illusion. If Bias: It is a permanent biological Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Meta-Cognitive Training (Double-Loop Learning)
 * Viability: Actively teaching people *how* to measure their own 
 * ignorance can mitigate the effect.
 * Suppression: Low in education, but high in corporate cultures that 
 * reward "fake it till you make it."
 * * ALTERNATIVE 2: External Auditing/Credentialing
 * Viability: Relying on objective external metrics rather than self-assessment.
 * * CONCLUSION:
 * The Dunning-Kruger Effect is a Mountain of the "First Person" experience, 
 * but it can be converted into a Rope through the "Third Person" 
 * perspective of auditing and education.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraint_dunning_kruger_effect].
 * 2. Multi-perspective: ?- multi_index_report(dunning_kruger_effect).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
