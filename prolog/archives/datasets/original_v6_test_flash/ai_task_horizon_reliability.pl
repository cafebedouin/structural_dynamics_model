% ============================================================================
% CONSTRAINT STORY: ai_task_horizon_reliability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_task_horizon_reliability, []).

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
 *   constraint_id: ai_task_horizon_reliability
 *   human_readable: The AI Task Horizon and Reliability Bottleneck
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint reflects the trade-off between the increasing complexity
 *   of tasks that AI systems are attempting to solve and the decreasing
 *   reliability of these systems as task complexity increases. As AI is
 *   applied to more intricate and demanding problems, the potential for
 *   errors, biases, and unpredictable failures rises, creating a structural
 *   tension that impacts various stakeholders.
 *
 * KEY AGENTS:
 *   - AI Developers: Primary beneficiaries (institutional/arbitrage) - Gain funding and market share.
 *   - Downstream Users: Primary victims (powerless/trapped) - Suffer from unreliable AI outputs.
 *   - Early Adopters: Moderate participants (moderate/constrained) - Benefit from competitive edge but face risks of unreliability.
 *   - Reliability of AI Systems: Collective victim (powerless/trapped) - The trustworthiness of AI is damaged by repeated failures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_task_horizon_reliability, 0.55).
domain_priors:suppression_score(ai_task_horizon_reliability, 0.4).
domain_priors:theater_ratio(ai_task_horizon_reliability, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_task_horizon_reliability, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_task_horizon_reliability, tangled_rope).
narrative_ontology:human_readable(ai_task_horizon_reliability, "The AI Task Horizon and Reliability Bottleneck").
narrative_ontology:topic_domain(ai_task_horizon_reliability, "technological/economic").

domain_priors:requires_active_enforcement(ai_task_horizon_reliability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_task_horizon_reliability, ai_developers).
narrative_ontology:constraint_beneficiary(ai_task_horizon_reliability, early_adopters).
narrative_ontology:constraint_victim(ai_task_horizon_reliability, downstream_users).
narrative_ontology:constraint_victim(ai_task_horizon_reliability, reliability_of_ai_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Downstream users of AI systems are often trapped by their reliance on these systems, with limited ability to verify or understand their outputs, especially for complex tasks. They bear the cost of unreliable AI predictions or decisions.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% AI developers benefit from the increased adoption of AI systems, gaining funding, recognition, and market share. They can often arbitrage different AI models or approaches, mitigating the impact of reliability issues on any single project.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Early adopters of AI systems are both beneficiaries and victims. They gain a competitive edge by using AI for complex tasks, but they also face the risk of system unreliability, leading to potential losses or inefficiencies. Their exit options are constrained by the need to maintain their competitive advantage.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, the observer sees the tangled rope of increasing AI capabilities coupled with the increasing potential for unpredictable failures, requiring careful management and oversight. The constraint is a complex interplay of coordination and extraction.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_task_horizon_reliability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_task_horizon_reliability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_task_horizon_reliability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The constraint extracts value from downstream users who rely on AI systems, especially as task complexity increases. Suppression (0.40): Moderate. There are limited alternatives to using AI in many complex applications, creating a degree of suppression of choices for users. Theater Ratio (0.30): Relatively low. While there is marketing and hype around AI, the actual performative aspect is not as high as in other domains, as practical application and tangible results are prioritized.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises because AI developers benefit from increased adoption and can arbitrage their risk, while downstream users are often trapped by their reliance on AI and bear the cost of unreliability. Early adopters experience a mix of both benefits and risks, constrained by the need to stay competitive.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers benefit from the adoption of AI (low d). Downstream users, particularly those with limited alternatives, bear the cost of unreliable AI (high d). Early adopters have a mixed relationship, gaining competitive advantages but also facing risks (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that the inverse relationship between task complexity and AI reliability can be viewed as a coordination challenge (rope) for developers who are constantly improving their systems, while also being an extractive process (snare) for users who are forced to rely on unreliable systems. The tangled rope perspective captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    task_complexity_metric,
    'How should task complexity be accurately and consistently measured across different domains?',
    'Develop a standardized metric for task complexity based on human time-to-complete, data requirements, and cognitive load.',
    'A better metric could refine the extractiveness score and improve the prediction of AI system reliability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(task_complexity_metric, empirical, 'Metric for measuring task complexity in AI systems.').

omega_variable(
    system_reliability_definition,
    'What constitutes acceptable reliability for AI systems in various critical applications?',
    'Establish domain-specific standards for AI reliability based on risk assessment, cost-benefit analysis, and ethical considerations.',
    'Clearer definitions of reliability could change the perception of the constraint from a snare to a more manageable risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_reliability_definition, preference, 'Defining acceptable reliability for AI systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_task_horizon_reliability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_t_tr_t0, ai_task_horizon_reliability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_t_tr_t5, ai_task_horizon_reliability, theater_ratio, 5, 0.2).
narrative_ontology:measurement(ai_t_tr_t10, ai_task_horizon_reliability, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_t_be_t0, ai_task_horizon_reliability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_t_be_t5, ai_task_horizon_reliability, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ai_t_be_t10, ai_task_horizon_reliability, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_task_horizon_reliability, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
