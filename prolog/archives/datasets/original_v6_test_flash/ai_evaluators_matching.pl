% ============================================================================
% CONSTRAINT STORY: ai_evaluators_matching
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_evaluators_matching, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_evaluators_matching
 *   human_readable: AI Talent Evaluators: Efficiency vs. Opacity in Automated Hiring
 *   domain: general
 *
 * SUMMARY:
 *   AI talent evaluators offer the promise of efficiency and objectivity in
 *   hiring, but also pose risks related to bias, opacity, and lack of human
 *   oversight. This constraint explores the tension between these competing
 *   forces and examines how different actors within the hiring ecosystem
 *   experience these AI systems. The rise of AI-driven hiring is intended to
 *   reduce costs and find optimal candidates, but can introduce subtle biases
 *   and reduce accountability.
 *
 * KEY AGENTS:
 *   - Job Applicants: Primary victims (powerless/trapped) - subject to the biases and limitations of AI algorithms
 *   - Hiring Companies: Primary beneficiaries (institutional/arbitrage) - gain efficiency and cost savings through automation
 *   - AI Evaluator Vendors: Powerful actor (powerful/arbitrage) - provide the technology and benefit from increased demand
 *   - HR Department: Moderate actor (moderate/constrained) - experience gains in efficiency, but potentially constrained in their understanding of evaluation logic
 *   - Underrepresented Groups: Specific sub-group of victims (powerless/trapped) - disproportionately affected by biased AI algorithms
 *   - Analytical Observer: Global observer (analytical/analytical) - analyzes the long-term societal implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_evaluators_matching, 0.55).
domain_priors:suppression_score(ai_evaluators_matching, 0.65).
domain_priors:theater_ratio(ai_evaluators_matching, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_evaluators_matching, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_evaluators_matching, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_evaluators_matching, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_evaluators_matching, tangled_rope).
narrative_ontology:human_readable(ai_evaluators_matching, "AI Talent Evaluators: Efficiency vs. Opacity in Automated Hiring").
narrative_ontology:topic_domain(ai_evaluators_matching, "general").

domain_priors:requires_active_enforcement(ai_evaluators_matching).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_evaluators_matching, hiring_companies).
narrative_ontology:constraint_beneficiary(ai_evaluators_matching, ai_evaluator_vendors).
narrative_ontology:constraint_victim(ai_evaluators_matching, job_applicants).
narrative_ontology:constraint_victim(ai_evaluators_matching, underrepresented_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Job applicants, particularly those from underrepresented groups, often lack the power to challenge the decisions made by AI evaluators. They are trapped within the system, subject to its biases and limitations. Their exit options are severely limited as they must navigate the AI-driven hiring landscape to secure employment.
constraint_indexing:constraint_classification(ai_evaluators_matching, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% HR departments benefit from increased efficiency in the short term, but are constrained in their understanding of the evaluator logic, long term, due to lack of transparency. They gain efficiency but lose agency in understanding and auditing the hiring process.
constraint_indexing:constraint_classification(ai_evaluators_matching, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Hiring companies benefit from the perceived efficiency and cost savings offered by AI talent evaluators. They can arbitrage between different AI vendors and methodologies, selecting the most advantageous options for their specific hiring needs.
constraint_indexing:constraint_classification(ai_evaluators_matching, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% AI evaluator vendors benefit from the demand for their services, but also face constraints related to regulatory compliance and maintaining fairness and transparency in their algorithms. They hold significant power but need to balance extraction with long-term sustainability and legal risks.
constraint_indexing:constraint_classification(ai_evaluators_matching, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical observers recognize the inherent trade-off between efficiency and opacity in AI talent evaluators. They see both the potential benefits and the risks associated with automated hiring processes, and advocate for transparency and accountability in AI-driven decision-making.
constraint_indexing:constraint_classification(ai_evaluators_matching, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_evaluators_matching_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_evaluators_matching, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_evaluators_matching, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_evaluators_matching, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_evaluators_matching_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. AI talent evaluators extract value from job applicants by filtering and ranking them based on potentially biased algorithms. The extraction is not total, as applicants can still find employment through other channels, but the AI-driven hiring landscape creates a significant barrier for many. Suppression (0.65): High. Job applicants have limited exit options and are subject to the decisions made by AI evaluators. The lack of transparency in these algorithms further suppresses their ability to challenge unfair or discriminatory outcomes. Theater Ratio (0.40): Moderate. While AI talent evaluators are marketed as objective and data-driven, there is a significant element of theater involved, as these systems are often black boxes and their effectiveness is difficult to verify. The theater emerges from the performative aspects of AI explainability and the challenge in truly understanding the rationale of the AI's decisions.
 *
 * PERSPECTIVAL GAP:
 *   Job applicants perceive the AI evaluators as a Snare, trapping them within a system that they have little power to influence or escape. Hiring companies see it as a Rope, enabling efficient and cost-effective hiring decisions. HR departments and AI evaluator vendors see it as a Tangled Rope, balancing the benefits of increased efficiency with the risks of bias and lack of transparency. The analytical observer recognizes the inherent trade-offs and advocates for transparency and accountability.
 *
 * DIRECTIONALITY LOGIC:
 *   Hiring companies and AI evaluator vendors benefit from the implementation of AI talent evaluators, while job applicants, especially those from underrepresented groups, bear the costs of potential biases and lack of transparency. HR is both a benefactor and target. Directionality reflects the structural positions of each group relative to the extractive process.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mandatrophy is resolved by the indexical classifications. While a naive perspective might view AI talent evaluators as purely beneficial (Rope) or purely detrimental (Snare), the DR framework reveals the complex interplay of coordination and extraction. The different perspectives highlight the need for a balanced approach that maximizes efficiency while minimizing bias and ensuring fairness for all job applicants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_transparency,
    'How can AI algorithms be made more transparent and auditable to ensure fairness and prevent discrimination?',
    'Development of explainable AI (XAI) techniques and independent audits of AI algorithms.',
    'Increased transparency could reduce bias and improve applicant trust, but may also reveal proprietary information and create opportunities for gaming the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_transparency, empirical, 'Addresses the question of algorithm transparency and its potential impact.').

omega_variable(
    human_oversight,
    'What level of human oversight is necessary to ensure that AI evaluators are used ethically and effectively?',
    'Establishing clear guidelines for human review and intervention in AI-driven hiring processes.',
    'Increased human oversight could improve fairness and prevent errors, but may also reduce efficiency and increase costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_oversight, preference, 'Determines the appropriate level of human oversight in AI evaluation.').

omega_variable(
    evaluation_metrics,
    'How can evaluation metrics be designed to capture the full range of skills and abilities relevant to job performance?',
    'Developing more holistic and comprehensive evaluation metrics that go beyond traditional measures of skills and experiences.',
    'Better metrics could lead to more accurate and fair assessments of job applicants, but may also be more difficult and costly to implement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evaluation_metrics, conceptual, 'Focuses on improving the quality and scope of evaluation metrics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_evaluators_matching, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_e_tr_t0, ai_evaluators_matching, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_e_tr_t5, ai_evaluators_matching, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ai_e_tr_t10, ai_evaluators_matching, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_e_be_t0, ai_evaluators_matching, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_e_be_t5, ai_evaluators_matching, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ai_e_be_t10, ai_evaluators_matching, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_evaluators_matching, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
