% ============================================================================
% CONSTRAINT STORY: cascading_constraint_failure
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_cascading_constraint_failure, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cascading_constraint_failure
 * human_readable: The Dominos of Systemic Collapse
 * domain: technological/infrastructural/economic
 * * SUMMARY:
 * This constraint occurs when a system is composed of tightly coupled,
 * interdependent sub-constraints (Ropes). When one localized constraint
 * fails or is breached, it shifts the load to adjacent constraints,
 * triggering a recursive "cascade" that transforms the entire coordination
 * stack into a catastrophic Snare for those within it.
 * * KEY AGENTS:
 * - Infrastructure End-User: Subject (Powerless)
 * - Network Optimizer: Beneficiary (Institutional)
 * - Chaos Engineer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.89) because the failure siphons all remaining system
% utility into the "firefighting" and survival costs of the cascade.
domain_priors:base_extractiveness(cascading_constraint_failure, 0.89).
domain_priors:suppression_score(cascading_constraint_failure, 0.80).
domain_priors:theater_ratio(cascading_constraint_failure, 0.25). % Functional collapse; low performative overhead.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cascading_constraint_failure, extractiveness, 0.89).
narrative_ontology:constraint_metric(cascading_constraint_failure, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(cascading_constraint_failure, theater_ratio, 0.25).

% Constraint self-claim (what does the constraint claim to be?)
% The system is presented as a pure coordination mechanism for efficiency.
narrative_ontology:constraint_claim(cascading_constraint_failure, tangled_rope).
narrative_ontology:human_readable(cascading_constraint_failure, "The Dominos of Systemic Collapse").
narrative_ontology:topic_domain(cascading_constraint_failure, "technological/infrastructural/economic").

% Binary flags
% Required for Tangled Rope: The system's efficiency relies on enforcing
% tight coupling and standardized protocols, preventing resilient alternatives.
domain_priors:requires_active_enforcement(cascading_constraint_failure).

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(cascading_constraint_failure, network_operators).
narrative_ontology:constraint_victim(cascading_constraint_failure, infrastructure_end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: as each layer of infrastructure fails, their
% options for mitigation or exit vanish in real-time.
constraint_indexing:constraint_classification(cascading_constraint_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the tight coupling as a Rope—it is what provides
% peak efficiency and coordination during normal operations.
constraint_indexing:constraint_classification(cascading_constraint_failure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature of hyper-efficient coordination (Rope)
% inevitably resulting in total systemic extraction (Snare) upon failure.
constraint_indexing:constraint_classification(cascading_constraint_failure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascading_constraint_failure_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cascading_constraint_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascading_constraint_failure, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(cascading_constraint_failure, tangled_rope,
        context(agent_power(analytical), _, _, _)).

:- end_tests(cascading_constraint_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.89) reflects a "Mandatrophy" state where the
 * system's complexity has become a predatory debt that liquidates the subject
 * upon the first breach of coordination. The Tangled Rope classification is
 * critical because it captures the dual nature of the system: it is both a
 * genuine coordination mechanism (Rope) for its operators and a catastrophic
 * failure mode (Snare) for its users. The high suppression (0.80) comes from
 * the lack of resilient, loosely-coupled alternatives, which are often
 * engineered out in the name of efficiency.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] Resolved via the Tangled Rope classification. This
 * recognizes that the catastrophe is the "tangled" inverse of the system's
 * coordination capability—the more tightly coupled the Ropes, the more
 * extractive the Snare upon failure. The system avoids misclassification as a
 * pure Snare by acknowledging its genuine, if fragile, coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cascading_constraint_failure,
    'Can the system be "modularized" to introduce resilience without losing its core coordination benefit?',
    'Simulating the tradeoff between system resilience (decoupling) and throughput (coupling).',
    'If modularity is efficient: Snare of design. If modularity is impossible without collapse: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cascading_constraint_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This system likely began as a beneficial coordination mechanism whose
% fragility and extractive potential grew as complexity and coupling increased
% without corresponding investments in resilience.
%
% Theater ratio over time (remains low as the system is functionally focused):
narrative_ontology:measurement(ccf_tr_t0, cascading_constraint_failure, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ccf_tr_t5, cascading_constraint_failure, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ccf_tr_t10, cascading_constraint_failure, theater_ratio, 10, 0.25).

% Extraction over time (the potential cost of failure accumulates):
narrative_ontology:measurement(ccf_ex_t0, cascading_constraint_failure, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(ccf_ex_t5, cascading_constraint_failure, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ccf_ex_t10, cascading_constraint_failure, base_extractiveness, 10, 0.89).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system is a form of large-scale infrastructure.
narrative_ontology:coordination_type(cascading_constraint_failure, global_infrastructure).

% Network relationships (structural influence edges)
% The failure of tightly-coupled logistics systems can trigger this cascade.
narrative_ontology:affects_constraint(just_in_time_logistics, cascading_constraint_failure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */