% ============================================================================
% CONSTRAINT STORY: algorithmic_bias
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini
% Source: Sociotechnical Systems / Machine Learning Ethics
% Status: [RESOLVED MANDATROPHY]
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
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: algorithmic_bias
 * human_readable: Algorithmic Bias
 * domain: technological/social
 * temporal_scope: 2010-Present (Big Data Era)
 * spatial_scope: Global (Digital Infrastructure)
 * * SUMMARY:
 * Algorithmic bias occurs when machine learning systems produce systematically 
 * prejudiced results by automating historical inequities found in training data. 
 * It functions as a "math-washed" barrier that scales discrimination under a 
 * veneer of objectivity, making it significantly harder to challenge than 
 * human prejudice.
 * * KEY AGENTS:
 * - The Data Subject: Individual powerless; marginalized subjects whose life 
 * chances (credit, legal, employment) are governed by biased models.
 * - The Institutional Deployer: Institutional power; organizations using AI 
 * to automate decision-making at scale.
 * - The Data Scientist/Engineer: Individual moderate; creators who manage 
 * the coordination of the model but are often constrained by dataset priors.
 * - The Ethical Auditor: Analytical observer; researchers studying the 
 * long-term compounding of systemic inequity.
 * * NARRATIVE ARC:
 * What begins as a "hidden variable" in training data transforms into an 
 * unyielding Mountain of reality for the subject. For the institution, it is 
 * a Rope for coordination and efficiency, while for the auditor, it is a 
 * Snare systematically strangling social mobility.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (REVISED)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(algorithmic_bias, 0, 10).
narrative_ontology:constraint_claim(algorithmic_bias, snare).

% Base Properties
domain_priors:base_extractiveness(algorithmic_bias, 0.75).
domain_priors:suppression_score(algorithmic_bias, 0.70).
domain_priors:theater_ratio(algorithmic_bias, 0.62). % Measures performative "Fairness Dashboards"

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(algorithmic_bias, extractiveness, 0.75).
narrative_ontology:constraint_metric(algorithmic_bias, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(algorithmic_bias, theater_ratio, 0.62).

% Mandatory keys for classification engine v3.4
domain_priors:requires_active_enforcement(algorithmic_bias).

% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(algorithmic_bias, institutional_deployer).
narrative_ontology:constraint_victim(algorithmic_bias, marginalized_data_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */


/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE DATA SUBJECT - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless; subjects denied loans or flagged by predictive policing.
   WHEN: immediate; affecting their current survival and status.
   WHERE: trapped; cannot opt-out of the digital governance systems.
   SCOPE: national/global.
   
   WHY THIS CLASSIFICATION:
   For the subject, the bias is a Snare. It is a coercive trap where their 
   historical disadvantage is converted into a mathematical certainty of 
   failure. They are trapped in a feedback loop where the model's output 
   further justifies their exclusion.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    algorithmic_bias,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INSTITUTIONAL DEPLOYER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional; managers and risk officers at banks or government agencies.
   WHEN: biographical; focused on long-term efficiency and scalability.
   WHERE: arbitrage; can choose different models or vendors.
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   For the institution, the model is a Rope—a functional tool for coordination. 
   It allows the processing of thousands of data points instantly. The bias 
   is often dismissed as "statistical noise" or a manageable trade-off 
   for the efficiency provided.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    algorithmic_bias,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DATA SCIENTIST - Tangled Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate; engineers who understand the technical flaws.
   WHEN: biographical; focused on professional career and technical standards.
   WHERE: constrained; limited by proprietary datasets and commercial pressure.
   SCOPE: national/global.
   
   WHY THIS CLASSIFICATION:
   For the engineer, this is a Tangled Rope. The system provides the 
   coordination to build powerful tools, but the embedded bias asymmetrically 
   extracts their professional ethics, forcing them to maintain a system they 
   know is flawed to achieve institutional goals.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    algorithmic_bias,
    tangled_rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE ETHICAL AUDITOR - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical; researchers and whistleblowers.
   WHEN: historical; observing long-term social compounding.
   WHERE: analytical; observer stance outside the corporate structure.
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   To the auditor, the bias is a Mountain. It represents an unchangeable 
   structural property of Big Data: as long as models are trained on 
   historical social data, they will treat that history as a natural law 
   of the future.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    algorithmic_bias,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(algorithmic_bias_tests).

test(multi_perspective_variance) :-
    % Subject (Snare) vs Institution (Rope) vs Auditor (Mountain)
    constraint_indexing:constraint_classification(algorithmic_bias, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_bias, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_bias, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_scaling) :-
    % Powerless subjects suffer high extraction while institutions manage it as a tool
    ContextPowerless = context(powerless, immediate, trapped, national),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(algorithmic_bias, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(algorithmic_bias, ContextPowerful, Score2),
    Score1 > Score2.

test(hybrid_tangled_rope_detection) :-
    % Ensure engineers recognize the coordination/extraction hybrid
    constraint_indexing:constraint_classification(algorithmic_bias, tangled_rope, context(individual_moderate, _, _, _)).

:- end_tests(algorithmic_bias_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini
 * Date: 2026-01-23
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Updated from 0.6 to 0.75 to reflect the mandatory Mandatrophy protocol. 
 * Algorithmic bias is highly extractive because it removes life-chances 
 * and mobility from entire classes without recourse.
 * * 2. PERSPECTIVE SELECTION:
 * Added the 'Tangled Rope' for the Data Scientist to show how their technical 
 * coordination is surgerically tied to ethical extraction.
 * * 3. MANDATROPHY RESOLUTION:
 * [RESOLVED MANDATROPHY] status is justified by showing that while the 
 * extraction is predatory for the subject (Snare), it remains a functional 
 * efficiency tool (Rope) for the institutional user.
 * * 4. AMBIGUITIES:
 * The primary ambiguity is whether "fairness" can be mathematically 
 * engineered or if the data itself remains an irreducible Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    algorithmic_bias_extraction_intent,
    "Is the systemic extraction a functional necessity of large-scale predictive modeling or a predatory choice to ignore human edge-cases?",
    resolution_mechanism("Audit of institutional resource allocation for bias mitigation vs. model deployment speed."),
    impact("If necessity: Mountain (natural law of data). If predatory choice: Snare (Mandatrophy)."),
    confidence_without_resolution(medium)
).

omega_variable(
    proxy_variable_irreducibility,
    "Can a model ever be unbiased if it uses socioeconomic data that is the product of historical bias?",
    resolution_mechanism("Mathematical proof of fairness impossibility theorems across all contexts."),
    impact("If Mountain: AI will always be a tool of status quo reinforcement. If Rope: We can engineer corrective bias."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Human-in-the-Loop (HITL)
 * Viability: High, provides oversight.
 * Suppression: High; institutions reject it because it destroys the ROI 
 * of automation.
 * * ALTERNATIVE 2: Algorithmic Reparations
 * Viability: Proactive weighting for marginalized groups.
 * Suppression: Extreme; often legally suppressed as "reverse discrimination" 
 *.
 * * CONCLUSION:
 * The active suppression of HITL alternatives in favor of pure automation 
 * confirms that for the Data Subject, this is a Snare.
 */

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising as "Ethical AI" rhetoric and performative audits (0.15) 
% replace actual structural fixes, reaching 0.62 at T=10.
narrative_ontology:measurement(bias_tr_t0, algorithmic_bias, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bias_tr_t5, algorithmic_bias, theater_ratio, 5, 0.38).
narrative_ontology:measurement(bias_tr_t10, algorithmic_bias, theater_ratio, 10, 0.62).

% Extraction: Progressive hardening of the "digital redlining" effect, 
% siphoning mobility from marginalized groups as models integrate into infrastructure.
narrative_ontology:measurement(bias_ex_t0, algorithmic_bias, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bias_ex_t5, algorithmic_bias, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(bias_ex_t10, algorithmic_bias, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [constraint_algorithmic_bias].
% Report: ?- multi_index_report(algorithmic_bias).
% Tests: ?- run_tests(algorithmic_bias_tests).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
