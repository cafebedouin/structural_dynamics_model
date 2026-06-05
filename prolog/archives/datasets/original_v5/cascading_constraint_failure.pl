% ============================================================================
% CONSTRAINT STORY: cascading_constraint_failure
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(cascading_constraint_failure, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3.

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

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cascading_constraint_failure, extractiveness, 0.89).
narrative_ontology:constraint_metric(cascading_constraint_failure, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(cascading_constraint_failure, theater_ratio, 0.25).

% This is an emergent property of high-entropy systems, not a scaffold.
% narrative_ontology:has_sunset_clause(cascading_constraint_failure). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
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
% Detects the hybrid nature of hyper-efficient coordination (Ropes) 
% inevitably resulting in total systemic extraction (Snare).
constraint_indexing:constraint_classification(cascading_constraint_failure, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(cascading_constraint_failure, E), E >= 0.50,
    domain_priors:suppression_score(cascading_constraint_failure, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascading_constraint_failure_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional optimizer.
    constraint_indexing:constraint_classification(cascading_constraint_failure, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascading_constraint_failure, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    % Ensure extraction (0.89) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(cascading_constraint_failure, E),

    E > 0.70.

:- end_tests(cascading_constraint_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.89) reflects a "Mandatrophy" state where the 
 * system's complexity has become a predatory debt that liquidates the subject 
 * upon the first breach of coordination.
 * 
 * * PERSPECTIVAL GAP:
 * The End-User feels a Snare because they are the terminal point of the 
 * cascade; they pay for the "efficiency" of the institution with their 
 * total lack of resilience. The Optimizer sees a Rope because the 
 * interdependence is what allows for the high-throughput coordination 
 * required for global scale.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes that 
 * the catastrophe is the "tangled" inverse of the system's coordination 
 * capability—the more tightly coupled the Ropes, the more extractive the Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_decoupling_possibility,
    'Can the system be "modularized" without losing its coordination benefit (Snare or Mountain)?',
    'Simulating the tradeoff between system resilience (decoupling) and throughput (coupling).',
    'If modularity is efficient: Snare of design. If modularity fails: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(cascading_constraint_failure, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
