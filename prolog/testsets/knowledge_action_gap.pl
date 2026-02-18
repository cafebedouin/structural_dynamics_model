% ============================================================================
% CONSTRAINT STORY: knowledge_action_gap
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_knowledge_action_gap, []).

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
 * * constraint_id: knowledge_action_gap
 * human_readable: The Informational Friction Barrier
 * domain: social/technological
 * * SUMMARY:
 * This constraint represents the structural disconnect between having access
 * to perfect information regarding a systemic risk (e.g., climate change,
 * public health) and the inability to coordinate a response due to high
 * switching costs, cognitive load, or entrenched habit-loops. It functions
 * as a 'Mountain' to individuals but a 'Tangled Rope' to systemic auditors.
 * * KEY AGENTS:
 * - Informed Individual: Subject (Powerless)
 * - Status Quo Systems: Beneficiary (Institutional)
 * - Future Generations: Victim
 * - Behavioral Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.71) because the gap effectively siphons optionality and future value.
domain_priors:base_extractiveness(knowledge_action_gap, 0.71).
% Suppression is moderate; alternatives exist but are costly to pursue.
domain_priors:suppression_score(knowledge_action_gap, 0.55).
domain_priors:theater_ratio(knowledge_action_gap, 0.25).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(knowledge_action_gap, extractiveness, 0.71).
narrative_ontology:constraint_metric(knowledge_action_gap, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(knowledge_action_gap, theater_ratio, 0.25).

% Constraint self-claim (what does the constraint claim to be?)
% It presents as an immutable fact of human nature ("people are just lazy/irrational").
narrative_ontology:constraint_claim(knowledge_action_gap, snare).
narrative_ontology:human_readable(knowledge_action_gap, "The Informational Friction Barrier").
narrative_ontology:topic_domain(knowledge_action_gap, "social/technological").

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(knowledge_action_gap). % Enforcement via cognitive/social friction.
narrative_ontology:constraint_beneficiary(knowledge_action_gap, status_quo_systems). % Derives has_coordination_function.
narrative_ontology:constraint_victim(knowledge_action_gap, future_generations). % Derives has_asymmetric_extraction.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the powerless individual, the gap feels like an unchangeable law of human nature.
% Effective extraction χ = 0.71 * 1.5 (powerless) * 1.0 (national) = 1.065.
% Despite the snare-level extraction, the feeling of immutability results in a Mountain classification.
constraint_indexing:constraint_classification(knowledge_action_gap, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutions view the gap as a 'Rope' that maintains predictability and stability
% by preventing "volatile" reactions to every new piece of information.
% Effective extraction χ = 0.71 * -0.2 (institutional) * 1.2 (global) = -0.17.
% The negative extraction reflects a perceived net benefit.
constraint_indexing:constraint_classification(knowledge_action_gap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% From an analytical distance, this gap is a snare that extracts future value
% by suppressing necessary immediate changes.
% Effective extraction χ = 0.71 * 1.15 (analytical) * 1.2 (global) = 0.9798.
constraint_indexing:constraint_classification(knowledge_action_gap, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% A deeper audit reveals the dual nature: it provides coordination (stability)
% while also performing asymmetric extraction, the hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(knowledge_action_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(knowledge_action_gap, E), E >= 0.50,
    domain_priors:suppression_score(knowledge_action_gap, S), S > 0.40,
    domain_priors:requires_active_enforcement(knowledge_action_gap).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_action_gap_tests).

test(perspectival_gap) :-
    % Verify the "Individual powerless" sees a Mountain while the "Institutional" sees a Rope.
    constraint_indexing:constraint_classification(knowledge_action_gap, mountain,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_action_gap, rope,
        context(agent_power(institutional), _, _, _)).

test(tangled_rope_conditions_met) :-
    % Verify that the analytical observer can classify it as a Tangled Rope.
    constraint_indexing:constraint_classification(knowledge_action_gap, tangled_rope,
        context(agent_power(analytical), time_horizon(civilizational), exit_options(arbitrage), spatial_scope(universal))).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(knowledge_action_gap, E),
    (E =< 0.15 ; E >= 0.46).

:- end_tests(knowledge_action_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction value (0.71) reflects the lost potential energy of un-applied
 * knowledge, a direct cost borne by future generations. The low theater ratio
 * (0.25) distinguishes this from a Piton. The key insight is that the constraint
 * is not just a bug; it's a feature that provides stability for existing systems,
 * making it a Tangled Rope. Enforcement is passive but powerful: the cognitive
 * and social costs of deviating from established norms.
 * * PERSPECTIVAL GAP:
 * The Individual feels the gap as a Mountain ("I know I should change, but
 * the world/my brain won't let me"). The Institution feels it as a Rope ("The gap
 * keeps the current system functional and coordinated"). The Analyst sees the
 * extraction and calls it a Snare, but the deeper audit reveals the coordination
 * function, resolving it to a Tangled Rope.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by acknowledging the Tangled Rope classification. The gap is
 * not pure extraction; it provides the 'latency' necessary for social
 * cohesion, even if that latency becomes predatory at the 0.71 threshold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required as extraction (0.71) > 0.46.
omega_variable(
    omega_cognitive_load,
    'Is the gap a hard neurological limit (Mountain) or a designed friction (Snare)?',
    'Introduction of sub-cognitive behavioral nudges in an isolated cohort.',
    'If gap remains: Mountain of biology. If gap closes: Snare of architecture.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(knowledge_action_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (0.71 > 0.46).
% Models the gap widening over time as information complexity increases and
% systems become more entrenched.
%
% Theater ratio over time (remains low, not degrading to Piton):
narrative_ontology:measurement(kag_tr_t0, knowledge_action_gap, theater_ratio, 0, 0.10).
narrative_ontology:measurement(kag_tr_t5, knowledge_action_gap, theater_ratio, 5, 0.18).
narrative_ontology:measurement(kag_tr_t10, knowledge_action_gap, theater_ratio, 10, 0.25).

% Extraction over time (shows intensification):
narrative_ontology:measurement(kag_ex_t0, knowledge_action_gap, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(kag_ex_t5, knowledge_action_gap, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(kag_ex_t10, knowledge_action_gap, base_extractiveness, 10, 0.71).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's coordination function is to manage and allocate finite
% cognitive and social resources, preventing systemic overload.
narrative_ontology:coordination_type(knowledge_action_gap, resource_allocation).

% The knowledge-action gap is a primary driver of institutional inertia,
% as it suppresses the impetus for organizational change.
narrative_ontology:affects_constraint(knowledge_action_gap, institutional_inertia).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */