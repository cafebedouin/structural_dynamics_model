% ============================================================================
% CONSTRAINT STORY: cognitive_induction_gap
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "One and One Sometimes Equals Eleven" by cafebedouin.org
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_cognitive_induction_gap, []).

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
 * 
 * constraint_id: cognitive_induction_gap
 * human_readable: The Induction Gap (Cognitive Compromise)
 * domain: psychological/social
 * temporal_scope: Perennial / Immediate
 * spatial_scope: Global / Cognitive
 * 
 * SUMMARY:
 * This constraint defines the "problem of induction" as a fundamental lack of rational basis for applying 
 * experienced circumstances to new, unexperienced situations. While humans use 
 * "intuitive leaps" to navigate this, factors such as high stress, social suasion, and investment in 
 * outcomes can compromise this ability, leading to a state where the subject is "completely unmoored" 
 * from reality.
 * 
 * KEY AGENTS:
 * - The Stressed Decision Maker (Individual Powerless): Individual facing high-stakes decisions under pressure.
 * - Military Command / Crisis Management Team (Institutional): Relies on intuitive leaps in fast-paced situations.
 * - The Practical Intuitive (Individual Moderate): Uses guesswork and feedback to improve decision-making.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(cognitive_induction_gap, 0, 10).
narrative_ontology:constraint_claim(cognitive_induction_gap, tangled_rope).

% Base extractiveness: 0.7.
% Cognitive compromise extracts the subject's ability to see reality 
% for the sake of group cohesion or emotional investment.
domain_priors:base_extractiveness(cognitive_induction_gap, 0.7).

% Suppression score: 0.6.
% "Noise" and "social suasion" suppress essential signal and individual dissent,
% making the compromise hard to realize.
domain_priors:suppression_score(cognitive_induction_gap, 0.6).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cognitive_induction_gap, extractiveness, 0.7).
narrative_ontology:constraint_metric(cognitive_induction_gap, suppression_requirement, 0.6).

% Enforcement: Emerges naturally from biological stress responses and group dynamics.
domain_priors:emerges_naturally(cognitive_induction_gap).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(cognitive_induction_gap, group_cohesion).
constraint_victim(cognitive_induction_gap, individual_judgment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STRESSED DECISION MAKER - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Slaves to high stress and "noise")
   WHEN: immediate (Must make a decision "quickly")
   WHERE: trapped (No rational basis for induction; unmoored)
   
   WHY THIS CLASSIFICATION:
   For the individual in a "high stress situation" with "too much information,"
   the constraint is a 'Snare.' Their cognitive capacity is extracted by
   pressure and noise, strangling their ability to make effective intuitive
   leaps and leading to poor decisions.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_induction_gap,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MILITARY COMMAND / CRISIS MANAGEMENT TEAM - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Relies on intuitive leaps in fast-paced situations)
   WHEN: immediate (Rapid decision-making in high-stakes environments)
   WHERE: arbitrage (Must actively mitigate biases and social suasion)
   
   WHY THIS CLASSIFICATION:
   For a military command or crisis management team, managing the cognitive
   induction gap is a 'Tangled Rope'. It's a 'Rope' because they rely on intuitive
   leaps in fast-paced situations to coordinate action. It's 'Tangled' because
   they must actively mitigate biases and social suasion within the team to
   avoid catastrophic errors, constantly balancing speed with accuracy.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_induction_gap,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PRACTICAL INTUITIVE - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Uses guesswork and guesswork feedback)
   WHEN: biographical (Tracking decisions and outcomes over time)
   WHERE: mobile (Adjusts assumptions based on experience)
   
   WHY THIS CLASSIFICATION:
   For the agent who "tracks how well your decisions do," the gap is a 'Rope.' 
   It is a functional coordination mechanism that allows them to make intuitive
   leaps that are "good enough" by using feedback as a navigational tool.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_induction_gap,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cognitive_induction_gap_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(cognitive_induction_gap, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_induction_gap, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_induction_gap, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(cognitive_induction_gap_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'Military Command / Crisis Management Team'
 *    as the institutional agent. This highlights how an organizational entity
 *    deals with the induction gap in high-stakes environments, making it a
 *    'Tangled Rope'.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Stressed Decision Maker (Snare): Cognitive capacity strangled by pressure.
 *    - Military Command (Tangled Rope): Balances intuitive leaps with bias mitigation.
 *    - Practical Intuitive (Rope): Uses feedback to refine intuitive leaps.
 * 
 * 3. CORE INSIGHT: The cognitive induction gap is an inherent 'Mountain' in
 *    human reasoning. While it can be a 'Snare' in high-stress situations,
 *    it can be navigated as a 'Rope' through feedback and becomes a 'Tangled Rope'
 *    for institutions that must manage collective decision-making under uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the fundamental nature of inductive reasoning and its limits.
 */

omega_variable(
    induction_logic_validity,
    "Is there a yet-undiscovered 'Rope' of rational induction, or is the 'Mountain' of logical isolation truly absolute, making all inductive leaps inherently flawed?",
    resolution_mechanism("Development of a formal system that demonstrably solves the problem of induction; philosophical advancements in epistemology."),
    impact("If Rope exists: Cognitive compromise is a remediable 'Snare'. If Mountain: It is a fundamental biological limit."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Scientific / Data-Driven Rigor
 *    Viability: Paring down information to what is essential through rigorous scientific method and statistical analysis.
 *    Suppression: Suppressed in fast-paced, high-stress environments where there's a "need to make a decision quickly" and limited data.
 *
 * CONCLUSION:
 * The cognitive induction gap highlights a fundamental trade-off: rigorous,
 * data-driven analysis (a 'Rope' for accuracy) is often suppressed by the
 * immediate need for intuitive leaps in high-stakes environments. The failure
 * to manage this trade-off turns the intuitive 'Rope' into a 'Snare'.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/cognitive_induction_gap].
 * 2. Multi-perspective: ?- multi_index_report(cognitive_induction_gap).
 * 3. Run tests: ?- run_tests(cognitive_induction_gap_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */