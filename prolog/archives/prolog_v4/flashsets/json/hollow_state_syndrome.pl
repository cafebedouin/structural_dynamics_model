% ============================================================================
% CONSTRAINT STORY: hollow_state_syndrome
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hollow_state_syndrome, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: hollow_state_syndrome
 *   human_readable: The Shell Governance Mirage
 *   domain: political/organizational
 *
 * SUMMARY:
 *   The 'Hollow State Syndrome' describes a scenario where a state maintains
 *   the formal appearance and legal authority of governance while having
 *   outsourced its core functions to private contractors or non-state actors.
 *   This leads to reduced accountability, potential corruption, and erosion
 *   of public services. The syndrome is characterized by an increase in
 *   'theater' as performative governance substitutes for functional control.
 *
 * KEY AGENTS:
 *   - Taxpayers: Primary target (powerless/trapped) - bears costs of reduced accountability and service quality.
 *   - Private Contractors: Primary beneficiary (institutional/arbitrage) - benefits from acquired contracts and influence.
 *   - Incumbent Politicians: Secondary beneficiary (powerful/constrained) - benefits from reduced direct responsibility but is also constrained by accountability issues.
 *   - Civil Servants: Affected party (moderate/constrained) - experiences erosion of roles and responsibilities.
 *   - Analytical Observer: Independent analyst (analytical/analytical) - sees mixed coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hollow_state_syndrome, 0.65).
domain_priors:suppression_score(hollow_state_syndrome, 0.7).
domain_priors:theater_ratio(hollow_state_syndrome, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hollow_state_syndrome, extractiveness, 0.65).
narrative_ontology:constraint_metric(hollow_state_syndrome, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hollow_state_syndrome, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hollow_state_syndrome, tangled_rope).
narrative_ontology:human_readable(hollow_state_syndrome, "The Shell Governance Mirage").
narrative_ontology:topic_domain(hollow_state_syndrome, "political/organizational").

domain_priors:requires_active_enforcement(hollow_state_syndrome).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hollow_state_syndrome, private_contractors).
narrative_ontology:constraint_beneficiary(hollow_state_syndrome, incumbent_politicians).
narrative_ontology:constraint_victim(hollow_state_syndrome, taxpayers).
narrative_ontology:constraint_victim(hollow_state_syndrome, civil_servants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The taxpayers are trapped within the system, bearing the costs of reduced accountability and potential corruption without easy means of exit or redress.
constraint_indexing:constraint_classification(hollow_state_syndrome, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Civil servants experience the erosion of their roles and responsibilities, constrained by the outsourcing arrangements and maintaining the appearance of governance, thus seeing the piton-like nature of the system.
constraint_indexing:constraint_classification(hollow_state_syndrome, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Private contractors benefit from the arrangement, acquiring contracts and influence, and perceive the system as a form of coordination (rope) that facilitates their operations.
constraint_indexing:constraint_classification(hollow_state_syndrome, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Incumbent politicians benefit from reduced direct responsibility and potential for patronage, but are also constrained by the system's inherent opacity and accountability issues.
constraint_indexing:constraint_classification(hollow_state_syndrome, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical observer sees a mixed system (tangled rope) where hollow state mechanisms extract from the public while also providing some functions (often at inflated cost or reduced accountability).
constraint_indexing:constraint_classification(hollow_state_syndrome, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hollow_state_syndrome_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hollow_state_syndrome, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hollow_state_syndrome, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hollow_state_syndrome, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hollow_state_syndrome, TR),
    TR >= 0.70.

:- end_tests(hollow_state_syndrome_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Outsourcing extracts resources from the public sector and transfers them to private contractors, often at inflated costs or with reduced service quality. Suppression (0.70): High. The system suppresses alternative governance models and reduces public participation in decision-making. Theater ratio (0.80): High. A significant portion of state activity becomes performative, focused on maintaining the appearance of governance rather than ensuring effective control.
 *
 * PERSPECTIVAL GAP:
 *   The taxpayers perceive the system as a snare, bearing the costs of reduced accountability. Civil servants perceive a piton, as their roles degrade and are replaced. Private contractors perceive a rope, benefiting from the arrangement. Incumbent politicians experience the system as a tangled rope, benefiting from reduced responsibility but also facing accountability issues. The analytical observer sees a mixed system (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural position of each agent within the system. Private contractors benefit (low d), while the taxpayers bear the costs (high d). The high theater ratio reflects the performative nature of governance. The analytical observer provides a balanced view of the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accountability_threshold,
    'At what level of outsourcing does the state lose effective control and accountability?',
    'Comparative analysis of governance outcomes in states with varying degrees of outsourcing.',
    'Determines the point at which the ''shell'' becomes dominant over the ''governance''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_threshold, empirical, 'Threshold for accountability loss due to outsourcing.').

omega_variable(
    service_quality_tradeoff,
    'What is the tradeoff between cost savings and service quality in outsourced functions?',
    'Cost-benefit analysis of outsourced vs. in-house service provision.',
    'Informs whether the system primarily serves private interests or public needs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_quality_tradeoff, empirical, 'Tradeoff between cost savings and service quality in outsourcing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hollow_state_syndrome, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(holl_tr_t0, hollow_state_syndrome, theater_ratio, 0, 0.5).
narrative_ontology:measurement(holl_tr_t5, hollow_state_syndrome, theater_ratio, 5, 0.7).
narrative_ontology:measurement(holl_tr_t10, hollow_state_syndrome, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(holl_be_t0, hollow_state_syndrome, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(holl_be_t5, hollow_state_syndrome, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(holl_be_t10, hollow_state_syndrome, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hollow_state_syndrome, resource_allocation).
narrative_ontology:affects_constraint(hollow_state_syndrome, regulatory_capture).
narrative_ontology:affects_constraint(hollow_state_syndrome, revolving_door_influence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
