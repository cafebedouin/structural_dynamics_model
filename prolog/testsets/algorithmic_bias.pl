% ============================================================================
% CONSTRAINT STORY: algorithmic_bias
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Sociotechnical Systems / Machine Learning Ethics
% ============================================================================

:- module(constraint_algorithmic_bias, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: algorithmic_bias
 * human_readable: Algorithmic Bias
 * domain: technological/social
 * temporal_scope: 2010-Present (The Era of Big Data)
 * spatial_scope: Global (Digital Infrastructure)
 * * SUMMARY:
 * Algorithmic bias occurs when computer systems, particularly those using 
 * machine learning, produce systematically prejudiced results. This constraint 
 * operates through training data that reflects historical inequities, which 
 * the algorithm then automates and scales, often under a veneer of 
 * "mathematical objectivity."
 * * KEY AGENTS:
 * - The Data Subject: The individual (often from a marginalized group) 
 * whose life chances are governed by the model.
 * - The Data Scientist: The creator who may be unaware of embedded priors 
 * or lacks the power to change the dataset.
 * - The Institutional Deployer: The organization (bank, court, employer) 
 * using the tool to automate decision-making.
 * * NARRATIVE ARC:
 * The constraint starts as a "hidden variable" in training data. Once deployed, 
 * it functions as an invisible barrier (Mountain) for those it discriminates 
 * against, while acting as an efficiency tool (Rope) for the institution. 
 * Because the bias is "math-washed," it is harder to challenge than human 
 * prejudice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(algo_bias_interval, 0, 10).
narrative_ontology:constraint_claim(algorithmic_bias, noose).

% Base extractiveness: 0.6 (High)
% Rationale: It extracts opportunity, credit, or freedom from the "victim" 
% class and converts it into administrative efficiency or profit for the 
% "beneficiary" class.
domain_priors:base_extractiveness(algorithmic_bias, 0.6).

% Suppression score: 0.7 (High)
% Rationale: The inner workings of these models are often protected as 
% trade secrets, and the outputs are presented as "objective," which 
% suppresses the visibility of the bias and the possibility of appeal.
domain_priors:suppression_score(algorithmic_bias, 0.7).

% Enforcement: Emerges naturally from historical data but requires active 
% institutional maintenance to ignore feedback.
domain_priors:requires_active_enforcement(algorithmic_bias).

% Metrics for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(algorithmic_bias, extractiveness, 0.6).
narrative_ontology:constraint_metric(algorithmic_bias, suppression_requirement, 0.7).

% Beneficiaries: Institutions seeking low-cost automation and "objective" 
% cover for gatekeeping.
constraint_beneficiary(algorithmic_bias, institutional_deployer).

% Victims: Marginalized groups represented poorly or unfairly in training data.
constraint_victim(algorithmic_bias, marginalized_data_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DATA SUBJECT (Powerless) - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A person denied a loan or flagged by police).
   WHEN: immediate (Affects their current life-state).
   WHERE: trapped (Cannot "opt-out" of the algorithmic gaze).
   SCOPE: national/global.
   
   WHY THIS CLASSIFICATION:
   For the subject, the bias is a Mountain. It is an immovable, opaque reality 
   that dictates their options. They cannot argue with the "black box," and 
   the systemic nature of the bias means it follows them across different 
   platforms and institutions.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    algorithmic_bias,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INSTITUTIONAL USER - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (The manager or risk officer).
   WHEN: biographical (Career-length focus on efficiency).
   WHERE: arbitrage (Can switch models or vendors).
   SCOPE: regional/national.
   
   WHY THIS CLASSIFICATION:
   For the institution, the algorithm (even with bias) is a Rope. It is a tool 
   for coordination and scale. It allows them to process thousands of 
   applicants instantly. The bias is often seen as a "margin of error" or a 
   "necessary trade-off" for the efficiency the tool provides.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    algorithmic_bias,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ETHICAL AUDITOR - NOOSE
   --------------------------------------------------------------------------
   
   WHO: analytical (A researcher or whistleblower).
   WHEN: historical (Viewing the long-term compounding of inequity).
   WHERE: constrained (Bound by NDA or limited access to the code).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   For the auditor, the bias is a Noose. They see how the system is designed 
   to pull tighter as it gathers more data. They recognize that the "efficiency" 
   of the Rope is actually a mechanism for extracting dignity and rights, 
   systematically strangling social mobility for specific classes.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    algorithmic_bias,
    noose,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(algorithmic_bias_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(algorithmic_bias, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_bias, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_bias, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_scaling) :-
    % Powerless subjects suffer the highest extractiveness (loss of opportunity).
    domain_priors:base_extractiveness(algorithmic_bias, E),
    E > 0.5.

test(suppression_visibility) :-
    % Suppression score should reflect the "black box" nature.
    domain_priors:suppression_score(algorithmic_bias, S),
    S >= 0.7.

:- end_tests(algorithmic_bias_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.6): High because it systematically removes 
 * autonomy and access from specific populations.
 * 2. NOOSE CLASSIFICATION: I chose Noose for the Auditor because they see 
 * the active suppression of alternatives (like manual review or diverse 
 * training sets) in favor of the biased model.
 * 3. SUPPRESSION: Set high (0.7) because algorithmic decisions are 
 * notoriously difficult to audit or explain to the layperson.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    proxy_variable_irreducibility,
    "Can a machine learning model ever be truly 'unbiased' if it uses 
    socioeconomic data that is itself the product of historical bias?",
    resolution_mechanism("Mathematical proof of 'fairness impossibility' 
    theorems in varied contexts"),
    impact("If Mountain: AI will always be a tool of status quo reinforcement. 
    If Rope: We can engineer 'corrective' bias."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Human-in-the-loop (HITL)
 * Viability: High, but expensive.
 * Suppression: High (Institutions reject it because it destroys the 
 * 'efficiency' ROI of the AI).
 * * ALTERNATIVE 2: Algorithmic Reparations (Proactive bias toward marginalized groups)
 * Viability: Mathematically possible but legally/politically controversial.
 * Suppression: Extreme (Often viewed as 'reverse discrimination').
 * * CONCLUSION:
 * The existence of Alternatives (HITL) that are suppressed for cost reasons 
 * confirms the Noose/Noose-like nature of the constraint for those within 
 * the system.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_algorithmic_bias].
 * 2. Report: ?- multi_index_report(algorithmic_bias).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
