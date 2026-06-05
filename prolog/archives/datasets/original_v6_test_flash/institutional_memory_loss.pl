% ============================================================================
% CONSTRAINT STORY: institutional_memory_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_memory_loss, []).

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
 *   constraint_id: institutional_memory_loss
 *   human_readable: The Amnesiac Organization
 *   domain: organizational/political/technological
 *
 * SUMMARY:
 *   The Amnesiac Organization describes a scenario where rapid personnel
 *   turnover, over-reliance on ephemeral digital communications, and the
 *   retirement of 'tacit knowledge' holders cause an institution to lose the
 *   'why' behind its own internal constraints. This results in policies and
 *   procedures being followed without understanding their original purpose,
 *   leading to inefficiency, unintended consequences, and a general erosion
 *   of organizational effectiveness.
 *
 * KEY AGENTS:
 *   - Short-Term Management: Primary beneficiary (institutional/arbitrage) - benefits from lack of historical context to implement changes quickly.
 *   - Long-Term Stakeholders: Primary victim (powerless/trapped) - suffers from poorly understood/abandoned constraints.
 *   - Future Employees: Secondary victim (moderate/constrained) - constrained by legacy systems they don't understand.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_memory_loss, 0.6).
domain_priors:suppression_score(institutional_memory_loss, 0.7).
domain_priors:theater_ratio(institutional_memory_loss, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_memory_loss, extractiveness, 0.6).
narrative_ontology:constraint_metric(institutional_memory_loss, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(institutional_memory_loss, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_memory_loss, piton).
narrative_ontology:human_readable(institutional_memory_loss, "The Amnesiac Organization").
narrative_ontology:topic_domain(institutional_memory_loss, "organizational/political/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_memory_loss, short_term_management).
narrative_ontology:constraint_victim(institutional_memory_loss, long_term_stakeholders).
narrative_ontology:constraint_victim(institutional_memory_loss, future_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Long-term stakeholders (e.g., future generations, dependent communities) are trapped by the organization's memory loss, as they suffer the consequences of poorly understood or abandoned constraints without the ability to influence or escape.
constraint_indexing:constraint_classification(institutional_memory_loss, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% New or future employees find themselves constrained by legacy systems and processes they don't understand, and which no one can adequately explain. While not entirely trapped, their career options are limited by their need for employment, and they must bear the costs of inefficient and opaque systems.
constraint_indexing:constraint_classification(institutional_memory_loss, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Short-term management benefits from the lack of institutional memory, as it allows them to implement changes without facing resistance based on historical context or long-term consequences. They can arbitrage this situation for personal gain or career advancement, experiencing it as a coordination mechanism.
constraint_indexing:constraint_classification(institutional_memory_loss, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% An analytical observer sees the organization as a Piton: a once-functional institution now operating largely on inertia and performative rituals, with the original purpose of its constraints forgotten. High theater ratio: policies are followed without understanding their rationale.
constraint_indexing:constraint_classification(institutional_memory_loss, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_memory_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_memory_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_memory_loss, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_memory_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_memory_loss, TR),
    TR >= 0.70.

:- end_tests(institutional_memory_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): A substantial portion of the organization's resources are effectively wasted due to the lack of understanding of the underlying constraints. Suppression (0.70): The lack of institutional memory suppresses alternative approaches and prevents effective problem-solving. Theater Ratio (0.80): A high proportion of the organization's activities are performative, with policies and procedures being followed without a clear understanding of their purpose.
 *
 * PERSPECTIVAL GAP:
 *   Short-term management sees the lack of historical context as an opportunity for arbitrage, while long-term stakeholders and future employees suffer the consequences of poorly understood and inefficient systems. The analytical observer sees a degraded institution operating largely on inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Short-term management benefits because they can make changes without constraints or accountability for long-term consequences (low d). Long-term stakeholders are victims because they bear the costs of poorly understood policies and missed objectives (high d). Future employees are constrained by outdated systems (high d). From an analytical viewpoint, the extraction flows towards future stake holders and away from the leadership.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memory_retrieval_feasibility,
    'Is it technically and economically feasible to retrieve and reconstruct the lost institutional memory?',
    'Conduct a comprehensive audit of existing records, interview former employees, and assess the potential for data recovery and knowledge reconstruction.',
    'If feasible: the organization can potentially restore its original constraints. If infeasible: the organization is likely to continue operating as a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memory_retrieval_feasibility, empirical, 'Feasibility of memory retrieval').

omega_variable(
    stakeholder_empowerment,
    'Can long-term stakeholders be empowered to hold the organization accountable for its actions and to advocate for the restoration of forgotten constraints?',
    'Implement mechanisms for stakeholder engagement, such as advisory boards, public forums, and independent audits.',
    'If stakeholders can be empowered: the organization may be forced to address its memory loss and restore forgotten constraints. If not: the organization is likely to continue prioritizing short-term gains over long-term sustainability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_empowerment, preference, 'Stakeholder empowerment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_memory_loss, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_memory_loss, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inst_tr_t5, institutional_memory_loss, theater_ratio, 5, 0.5).
narrative_ontology:measurement(inst_tr_t10, institutional_memory_loss, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_memory_loss, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(inst_be_t5, institutional_memory_loss, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(inst_be_t10, institutional_memory_loss, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_memory_loss, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
