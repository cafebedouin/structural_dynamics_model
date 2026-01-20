% ============================================================================
% CONSTRAINT STORY: burden_of_proof_scientific_empirical
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Statistical Significance and the Scientific Method (p < 0.05)
% ============================================================================

:- module(constraint_scientific_burden_of_proof, []).

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
 * * constraint_id: burden_of_proof_scientific_empirical
 * human_readable: Statistical Significance (The $p < 0.05$ Threshold)
 * domain: technological/social
 * temporal_scope: 1925-Present (Post-Fisherian Statistics)
 * spatial_scope: Global (Scientific Community)
 * * SUMMARY:
 * In the empirical sciences, the burden of proof is codified through 
 * statistical significance, typically requiring a p-value of less than 0.05 
 * to reject the null hypothesis. This variety of the burden is designed to 
 * filter out random noise and ensure that "claims to truth" are reproducible 
 * and grounded in mathematical probability rather than anecdotal observation.
 * * KEY AGENTS:
 * - The PhD Candidate: Individual powerless; must achieve the threshold to 
 * publish and graduate.
 * - The Journal Editor/Peer Reviewer: Institutional; enforcers of the 
 * statistical "gold standard."
 * - The Meta-Analyst: Analytical; examines the aggregate behavior of the 
 * threshold (e.g., the "replication crisis").
 * * NARRATIVE ARC:
 * Originally established as a Rope (a functional coordination tool for 
 * researchers), the $p < 0.05$ threshold has occasionally mutated into a 
 * Noose (p-hacking and publication bias). For the individual researcher, 
 * it remains a Mountain—an unyielding barrier that determines what 
 * enters the "record of truth."
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(scientific_revolution_current, 0, 10).
narrative_ontology:constraint_claim(burden_of_proof_scientific_empirical, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.3. Moderate extraction; it extracts significant time and 
% resources (failed experiments) to produce a single "truth" data point.
domain_priors:base_extractiveness(burden_of_proof_scientific_empirical, 0.3).

% Suppression score (0.0-1.0)
% Rationale: 0.6. Alternatives (Bayesian inference, effect sizes) are 
% visible but suppressed by the "prestige" and "standardization" of p-values.
domain_priors:suppression_score(burden_of_proof_scientific_empirical, 0.6).

% Enforcement requirements
% Requires active enforcement (Journal gatekeeping, funding requirements).
domain_priors:requires_active_enforcement(burden_of_proof_scientific_empirical).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, extractiveness, 0.3).
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(burden_of_proof_scientific_empirical, established_journals).
constraint_beneficiary(burden_of_proof_scientific_empirical, scientific_consensus_stability).
constraint_victim(burden_of_proof_scientific_empirical, novel_high_risk_hypotheses).
constraint_victim(burden_of_proof_scientific_empirical, null_result_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RESEARCHER - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the threshold for career advancement.
   WHEN: immediate - The duration of a single grant or study.
   WHERE: trapped - Bound by the statistical norms of their specific field.
   SCOPE: local - A single experiment or paper.
   
   WHY THIS CLASSIFICATION:
   To the researcher, $p < 0.05$ is a Mountain. You cannot "argue" with your 
   data set's p-value at the end of a study. If the value is 0.06, the path 
   to publication is blocked by an unchangeable feature of the institutional 
   landscape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_scientific_empirical,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(burden_of_proof_scientific_empirical),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SCIENTIFIC COMMUNITY - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Rule-making power through consensus and peer review.
   WHEN: generational - Building a body of knowledge over decades.
   WHERE: mobile - Can (and does) shift standards (e.g., moving to $p < 0.005$ in physics).
   SCOPE: global - Universal language of science.
   
   WHY THIS CLASSIFICATION:
   For the collective community, the burden of proof is a Rope—a necessary 
   coordination mechanism. It allows a chemist in Tokyo to evaluate a 
   study from Brazil using a shared metric of "trust," preventing the 
   system from being flooded by false positives.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_scientific_empirical,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(burden_of_proof_scientific_empirical, E),
    E < 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE META-ANALYST - Noose
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the system's aggregate failures.
   WHEN: historical - Examining the "replication crisis" of the 2010s-2020s.
   WHERE: analytical - Not incentivized to "publish or perish."
   SCOPE: global - Impact on the credibility of science as a whole.
   
   WHY THIS CLASSIFICATION:
   The meta-analyst identifies the Noose. The rigid $p < 0.05$ threshold 
   incentivizes p-hacking and the "file drawer problem" (suppression of 
   null results), extracting the actual truth of scientific inquiry to 
   satisfy the "extractive" requirement of novel results.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    burden_of_proof_scientific_empirical,
    noose,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(burden_of_proof_scientific_empirical, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(scientific_burden_of_proof_tests).

test(multi_perspective_gap) :-
    % Researcher (Mountain) vs Community (Rope) vs Meta-Analyst (Noose)
    constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, T1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, T2, context(institutional, generational, mobile, global)),
    constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(suppression_impact_on_truth) :-
    domain_priors:suppression_score(burden_of_proof_scientific_empirical, S),
    S > 0.5.

:- end_tests(scientific_burden_of_proof_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. DOMAIN ISOLATION: This is the **scientific/empirical** variety. It 
 * differs from the **legal** variety (which focuses on liberty) and the 
 * **engineering** variety (which focuses on safety) by focusing on 
 * **epistemic certainty**.
 * * 2. CLASSIFICATION: I primary-labeled this as 'Mountain' for the 
 * individual because of the unyielding nature of the "0.05" line in 
 * professional survival.
 * * 3. EXTRACTIVENESS: Set to 0.3; it is not purely extractive like the 
 * Challenger failure, but it does "waste" labor on studies that fall 
 * just outside the threshold.
 */

omega_variable(
    arbitrary_threshold_logic,
    "Is there a mathematical basis for 0.05, or is it a historical 'Mountain' 
     that has become a 'Noose' through inertia?",
    resolution_mechanism("Simulate scientific progress across various alpha-level thresholds (0.01, 0.05, 0.10)"),
    impact("If Arbitrary: The burden is a Noose. If Optimally Predictive: 
            It is a vital Rope."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Bayesian Inference (Credible Intervals)
 * Viability: High; allows for a continuous measure of belief rather than 
 * a binary pass/fail.
 * Suppression: Suppressed by the "Standard" infrastructure of statistical 
 * software, textbooks, and reviewer expectations.
 * * CONCLUSION:
 * The difficulty of switching to Bayesian methods, despite their technical 
 * advantages, shows that $p < 0.05$ has moved from a Rope to a Mountain 
 * for most researchers.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [burden_of_proof_scientific_empirical].
% 2. Analyze: ?- multi_index_report(burden_of_proof_scientific_empirical).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
