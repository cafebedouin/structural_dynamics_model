% ============================================================================
% CONSTRAINT STORY: planning_fallacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planning_fallacy, []).

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
 *   constraint_id: planning_fallacy
 *   human_readable: The Planning Fallacy
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The planning fallacy is a cognitive bias where predictions about the time
 *   needed to complete a task exhibit an optimism bias, underestimating the
 *   actual time required. This can lead to cost overruns, delays, and unmet
 *   expectations. Project promoters benefit from the initial enthusiasm,
 *   while end-users and future stakeholders often bear the costs of these
 *   miscalculations.
 *
 * KEY AGENTS:
 *   - Project Promoters: Primary beneficiary (institutional/arbitrage) - benefit from initial publicity and perceived progress.
 *   - End Users: Primary victim (powerless/trapped) - bear the costs of delays and unmet expectations.
 *   - Project Managers: Moderate actor (moderate/constrained) - constrained by deadlines but benefit from project success.
 *   - Future Stakeholders: Secondary victim (moderate/constrained) - bear costs due to potentially degraded system performance.
 *   - Initial Stakeholders: Secondary beneficiary (moderate/mobile) - may benefit from initial stages of project implementation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planning_fallacy, 0.5).
domain_priors:suppression_score(planning_fallacy, 0.6).
domain_priors:theater_ratio(planning_fallacy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planning_fallacy, extractiveness, 0.5).
narrative_ontology:constraint_metric(planning_fallacy, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(planning_fallacy, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planning_fallacy, tangled_rope).
narrative_ontology:human_readable(planning_fallacy, "The Planning Fallacy").
narrative_ontology:topic_domain(planning_fallacy, "economic/social/technological").

domain_priors:requires_active_enforcement(planning_fallacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planning_fallacy, project_promoters).
narrative_ontology:constraint_beneficiary(planning_fallacy, initial_stakeholders).
narrative_ontology:constraint_victim(planning_fallacy, end_users).
narrative_ontology:constraint_victim(planning_fallacy, future_stakeholders).
narrative_ontology:constraint_victim(planning_fallacy, overall_system_performance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% End users are often trapped as they rely on the completion of the project but have no control over its timeline. They bear the cost of delays and unmet expectations.
constraint_indexing:constraint_classification(planning_fallacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Project managers are constrained by the need to deliver projects on time and within budget, but also benefit from career advancement when projects are perceived as successful (even if the timeline was unrealistic). They experience both coordination and extraction.
constraint_indexing:constraint_classification(planning_fallacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Project promoters (e.g., politicians, CEOs) benefit from the initial positive publicity and perceived progress, regardless of whether the project is ultimately completed on time. They can often arbitrage their position to other opportunities.
constraint_indexing:constraint_classification(planning_fallacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the planning fallacy as a systemic issue that combines coordination (initial alignment on goals) and extraction (underestimation of costs borne by end users and future stakeholders).
constraint_indexing:constraint_classification(planning_fallacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planning_fallacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planning_fallacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planning_fallacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(planning_fallacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(planning_fallacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Moderate. The planning fallacy extracts resources (time, money, opportunity cost) from end-users and future stakeholders due to inaccurate planning.  Suppression (0.60): Moderate-High. There is often limited recourse for end-users once a project is underway, leading to a suppression of alternatives. Theater ratio (0.30): Low. There is some performative element in showcasing initial progress, but the core function of completing the project remains paramount.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises because project promoters, who are in a position to arbitrage their benefits, experience the fallacy as a coordinating mechanism to get a project off the ground. End-users, who are trapped with the consequences, experience it as extraction. Project managers experience both, as they are constrained by the imposed timeline but gain from perceived success. An analytical observer sees both the coordinating function of getting projects started and the extractive consequences for end-users and the system's overall performance.
 *
 * DIRECTIONALITY LOGIC:
 *   Project promoters benefit from initial enthusiasm (low d), while end-users bear the costs of delays (high d). Project managers are constrained by deadlines but also benefit from successful projects (moderate d). This leads to different classifications based on each agent's perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_assessment,
    'How accurately can the complexity of a project be assessed at the outset?',
    'Statistical analysis of similar projects, expert consultations, scenario planning.',
    'If complexity is accurately assessed, the planning fallacy''s impact is reduced. If complexity is underestimated, the fallacy becomes more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_assessment, empirical, 'Accuracy of initial project complexity assessment.').

omega_variable(
    incentive_alignment,
    'To what extent are incentives aligned between project promoters and end users?',
    'Analysis of contractual agreements, stakeholder engagement, and feedback mechanisms.',
    'If incentives are aligned, the planning fallacy''s extractive effect is reduced. If incentives are misaligned, the fallacy exacerbates negative consequences for end users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_alignment, conceptual, 'Alignment of incentives between project promoters and end users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planning_fallacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plan_tr_t0, planning_fallacy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(plan_tr_t5, planning_fallacy, theater_ratio, 5, 0.2).
narrative_ontology:measurement(plan_tr_t10, planning_fallacy, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(plan_be_t0, planning_fallacy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(plan_be_t5, planning_fallacy, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(plan_be_t10, planning_fallacy, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planning_fallacy, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
