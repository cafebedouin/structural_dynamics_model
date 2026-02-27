% ============================================================================
% CONSTRAINT STORY: emotional_cycles_of_change
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emotional_cycles_of_change, []).

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
 *   constraint_id: emotional_cycles_of_change
 *   human_readable: The Kelley-Connor Cycle of Change
 *   domain: psychology/behavioral_science
 *
 * SUMMARY:
 *   The Kelley-Connor Cycle of Change describes the emotional cost of
 *   adopting new systems or habits, progressing through stages of Uninformed
 *   Optimism, Informed Pessimism, Valley of Despair, Informed Optimism, and
 *   Success/Completion. This constraint highlights the inherent tension
 *   between the potential benefits of change and the emotional challenges
 *   faced by individuals undergoing it.
 *
 * KEY AGENTS:
 *   - Employees Undergoing Change: Primary target (powerless/trapped) - experience stress and uncertainty.
 *   - Organizations Adopting New Systems: Secondary target (moderate/constrained) - bear implementation costs and disruption.
 *   - Change Management Consultants: Primary beneficiary (institutional/arbitrage) - profit from assisting organizations with change.
 *   - System Vendors: Beneficiary - profit from adoption of their systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emotional_cycles_of_change, 0.5).
domain_priors:suppression_score(emotional_cycles_of_change, 0.6).
domain_priors:theater_ratio(emotional_cycles_of_change, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emotional_cycles_of_change, extractiveness, 0.5).
narrative_ontology:constraint_metric(emotional_cycles_of_change, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(emotional_cycles_of_change, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emotional_cycles_of_change, tangled_rope).
narrative_ontology:human_readable(emotional_cycles_of_change, "The Kelley-Connor Cycle of Change").
narrative_ontology:topic_domain(emotional_cycles_of_change, "psychology/behavioral_science").

domain_priors:requires_active_enforcement(emotional_cycles_of_change).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emotional_cycles_of_change, change_management_consultants).
narrative_ontology:constraint_beneficiary(emotional_cycles_of_change, system_vendors).
narrative_ontology:constraint_victim(emotional_cycles_of_change, employees_undergoing_change).
narrative_ontology:constraint_victim(emotional_cycles_of_change, organizations_adopting_new_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Employees often feel trapped during organizational changes, experiencing increased workload, uncertainty, and stress. They may lack the power to influence the process or exit the situation without significant personal cost.
constraint_indexing:constraint_classification(emotional_cycles_of_change, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Organizations benefit from adopting new systems but bear the cost of implementation, training, and potential disruption. They are constrained by budget, time, and the need to maintain operations during the transition.
constraint_indexing:constraint_classification(emotional_cycles_of_change, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Change management consultants benefit from the demand for their services during organizational transitions, providing expertise and guidance. They arbitrage their knowledge across multiple organizations, reducing the personal extraction.
constraint_indexing:constraint_classification(emotional_cycles_of_change, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% System vendors benefit from adoption of their systems; after the initial implementation, their role may become primarily theatrical; providing maintenance with diminishing returns, high theater ratio
constraint_indexing:constraint_classification(emotional_cycles_of_change, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The cycle represents a mix of coordination (new systems can improve efficiency) and extraction (emotional cost to employees). The cycle is actively reinforced by change management strategies that may not fully address the emotional impact on employees.
constraint_indexing:constraint_classification(emotional_cycles_of_change, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emotional_cycles_of_change_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emotional_cycles_of_change, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emotional_cycles_of_change, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emotional_cycles_of_change, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emotional_cycles_of_change, TR),
    TR >= 0.70.

:- end_tests(emotional_cycles_of_change_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Change initiatives extract emotional energy and time from employees, even if they ultimately improve efficiency. Suppression (0.60): Organizations often suppress resistance to change, discouraging open discussion of concerns. Theater Ratio (0.20): The focus is mainly on functional change, with less emphasis on performative aspects. Requires Active Enforcement: the active enforcement comes from internal pressure to adopt new systems and practices.
 *
 * PERSPECTIVAL GAP:
 *   Employees experience the change as a snare, where they have limited options and are subject to the process. Organizations see a more balanced picture, where they can at least influence the change process, and consultants see a rope, where they are benefiting from the transitions. The cycle describes a series of emotions that employees 'must' go through. Resistance is futile
 *
 * DIRECTIONALITY LOGIC:
 *   Change management consultants and system vendors benefit from the implementation of new systems, with multiple clients to leverage their expertise. The employees are subject to emotional upheaval with limited or no recourse
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_variability,
    'To what extent does individual personality and coping style affect the experience of the change cycle?',
    'Longitudinal studies tracking emotional responses to change across different personality types',
    'High variability would weaken the predictive power of the model; low variability would strengthen it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_variability, empirical, 'The impact of individual differences on emotional response to change.').

omega_variable(
    intervention_effectiveness,
    'How effectively can interventions mitigate the negative emotional stages of the cycle?',
    'Controlled trials comparing the emotional trajectories of groups receiving different interventions',
    'Highly effective interventions would reduce extractiveness; ineffective interventions would maintain or increase it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_effectiveness, empirical, 'The potential for interventions to reduce negative emotional impact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emotional_cycles_of_change, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emot_tr_t0, emotional_cycles_of_change, theater_ratio, 0, 0.1).
narrative_ontology:measurement(emot_tr_t5, emotional_cycles_of_change, theater_ratio, 5, 0.15).
narrative_ontology:measurement(emot_tr_t10, emotional_cycles_of_change, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(emot_be_t0, emotional_cycles_of_change, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(emot_be_t5, emotional_cycles_of_change, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(emot_be_t10, emotional_cycles_of_change, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emotional_cycles_of_change, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
