% ============================================================================
% CONSTRAINT STORY: dunning_kruger_effect
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
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
 * 
 * constraint_id: dunning_kruger_effect
 * human_readable: Dunning-Kruger Effect (Cognitive Bias of Self-Assessment)
 * domain: social/cognitive/educational
 * temporal_scope: Permanent (Cognitive Architecture)
 * spatial_scope: Global (Human Decision Making)
 * 
 * SUMMARY:
 * The Dunning-Kruger effect is a cognitive bias where people with limited 
 * competence in a domain overestimate their own ability. This occurs because 
 * the skills required for competence are the same skills needed to recognize 
 * incompetence. Conversely, high-performers often underestimate their 
 * relative superiority, assuming others find tasks as easy as they do.
 * 
 * KEY AGENTS:
 * - The Novice (Individual Powerless): Overconfident due to a lack of "meta-cognitive" awareness.
 * - The HR Department / Corporate Trainer (Institutional): Manages employee development.
 * - The Expert (Individual Powerful): Competent but prone to "false consensus" regarding others' ability.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(dunning_kruger_effect, 0, 10).
narrative_ontology:constraint_claim(dunning_kruger_effect, tangled_rope).

% Base extractiveness: 0.3.
% Overconfidence in incompetent actors extracts resources from the collective (errors) while 
% the actor extracts social status they haven't earned.
domain_priors:base_extractiveness(dunning_kruger_effect, 0.3).

% Suppression score: 0.7.
% The bias itself suppresses the visibility of the "competence" 
% alternative. One cannot see the alternative (being correct) because they 
% lack the tools to recognize their own error.
domain_priors:suppression_score(dunning_kruger_effect, 0.7).

% Enforcement: Emerges naturally from human cognitive architecture.
domain_priors:emerges_naturally(dunning_kruger_effect).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(dunning_kruger_effect, overconfident_novices).
constraint_victim(dunning_kruger_effect, organizational_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE NOVICE - Rope
   --------------------------------------------------------------------------
   WHO: individual_powerless (Lacks meta-cognitive awareness)
   WHEN: immediate (Early stages of learning a new skill)
   WHERE: mobile (Unwarranted confidence allows them to take risks and start learning)
   
   WHY THIS CLASSIFICATION:
   For the novice, a degree of Dunning-Kruger is a 'Rope'. It provides
   the "delusional" optimism required to coordinate resources and start a 
   project that an expert might deem too difficult. This unwarranted 
   confidence acts as a tether to a future reality they are trying to build.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dunning_kruger_effect,
    rope,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE HR DEPARTMENT / CORPORATE TRAINER - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Manages employee development and performance)
   WHEN: biographical (Long-term employee growth)
   WHERE: arbitrage (Balances individual perception with objective metrics)
   
   WHY THIS CLASSIFICATION:
   For an HR department, the Dunning-Kruger effect is a 'Tangled Rope'. It's a 'Rope'
   because understanding it helps identify skill gaps and manage employee expectations.
   It's 'Tangled' because they constantly struggle with overconfident, incompetent
   employees and underconfident, competent ones, leading to inefficiencies and frustration.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dunning_kruger_effect,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EXPERT - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerful (High competence in a domain)
   WHEN: immediate (Dealing with an overconfident, incompetent subordinate)
   WHERE: constrained (Cannot delegate critical tasks without risk)
   
   WHY THIS CLASSIFICATION:
   For the expert, the Dunning-Kruger effect is a 'Snare'. They are often frustrated
   by the overconfidence of incompetent individuals, leading to wasted time correcting
   errors or managing preventable crises. Their ability to efficiently execute is
   strangled by others' lack of self-awareness.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dunning_kruger_effect,
    snare,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(dunning_kruger_effect_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(dunning_kruger_effect, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(dunning_kruger_effect, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(dunning_kruger_effect, Type3, context(agent_power(individual_powerful), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(dunning_kruger_effect_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. INSTITUTIONAL PERSPECTIVE: Added the 'HR Department / Corporate Trainer'
 *    as the institutional agent. This highlights the practical challenges of
 *    managing talent development and performance in an organizational context.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Novice (Rope): Unwarranted confidence as a starting mechanism.
 *    - HR (Tangled Rope): Manages the conflicting realities of self-assessment.
 *    - Expert (Snare): Frustrated by the incompetence of others.
 * 
 * 3. CORE INSIGHT: The Dunning-Kruger effect is a fundamental cognitive bias
 *    that creates pervasive challenges in skill assessment and organizational
 *    efficiency, often leading to a 'Tangled Rope' for those managing talent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is whether the effect is a fundamental bias or a statistical artifact.
 */

omega_variable(
    statistical_regression_artifact,
    "Is the Dunning-Kruger effect a fundamental cognitive bias or a mathematical artifact of regression toward the mean?",
    resolution_mechanism("Replication of studies using non-traditional statistical models, coupled with neurological imaging of self-assessment processes."),
    impact("If Artifact: The 'Mountain' of cognitive architecture is an illusion. If Bias: It is a permanent biological 'Snare'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Meta-Cognitive Training (Double-Loop Learning)
 *    Viability: Actively teaching people *how* to measure their own ignorance can mitigate the effect.
 *    Suppression: Low in education, but high in corporate cultures that reward
 *    "fake it till you make it" or where leadership cannot admit their own limits.
 *
 * CONCLUSION:
 * While the Dunning-Kruger Effect is a natural 'Mountain' of human cognition,
 * it can be mitigated by active interventions. The failure to implement such
 * training often leads to it functioning as a 'Snare' for organizational efficiency.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/dunning_kruger_effect].
 * 2. Multi-perspective: ?- multi_index_report(dunning_kruger_effect).
 * 3. Run tests: ?- run_tests(dunning_kruger_effect_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */