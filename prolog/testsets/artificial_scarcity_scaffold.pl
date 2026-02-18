% ============================================================================
% CONSTRAINT STORY: artificial_scarcity_scaffold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2023-10-27
% ============================================================================

:- module(constraint_artificial_scarcity_scaffold, []).

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
 * * constraint_id: artificial_scarcity_scaffold
 * human_readable: The Resource-Migration Scaffold
 * domain: technological/economic
 * * SUMMARY:
 * This constraint represents a temporary period of enforced artificial scarcity
 * designed to facilitate a transition from a legacy resource to a new,
 * abundant alternative. It functions as a coordination mechanism that
 * temporarily suppresses choice to prevent a "tragedy of the commons"
 * during the migration phase, ensuring the new system reaches critical mass.
 * * KEY AGENTS:
 * - Resource Consumer (Victim): Subject (Powerless)
 * - System Beneficiaries (e.g., adopters of the new resource): Beneficiary (Institutional)
 * - Systems Architect: Architect (Organized)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(artificial_scarcity_scaffold, 0.48).
domain_priors:suppression_score(artificial_scarcity_scaffold, 0.75).
domain_priors:theater_ratio(artificial_scarcity_scaffold, 0.20).
domain_priors:requires_active_enforcement(artificial_scarcity_scaffold).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, extractiveness, 0.48).
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(artificial_scarcity_scaffold, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(artificial_scarcity_scaffold, scaffold).
narrative_ontology:human_readable(artificial_scarcity_scaffold, "The Resource-Migration Scaffold").
narrative_ontology:topic_domain(artificial_scarcity_scaffold, "technological/economic").

% Binary flags
% Mandatory for Scaffold classification: A sunset clause exists for the transition.
narrative_ontology:has_sunset_clause(artificial_scarcity_scaffold).

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2 (Required for Scaffold)
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(artificial_scarcity_scaffold, system_beneficiaries).
narrative_ontology:constraint_victim(artificial_scarcity_scaffold, resource_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the powerless agent, the high cost and lack of choice feel like a predatory trap.
% χ = 0.48 * 1.5 (powerless) * 1.0 (national) = 0.72, which is a Snare (>= 0.66).
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views this as a vital coordination Rope for systemic stability.
% χ = 0.48 * -0.2 (institutional) * 1.2 (global) = -0.115, which is a Rope.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ARCHITECT (SCAFFOLD)
% Suppression is tolerated because it is temporary and linked to a sunset clause.
constraint_indexing:constraint_classification(artificial_scarcity_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(artificial_scarcity_scaffold).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(artificial_scarcity_scaffold_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(artificial_scarcity_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(artificial_scarcity_scaffold, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(scaffold_trigger) :-
    % Verify that the presence of a sunset clause triggers the Scaffold classification.
    constraint_indexing:constraint_classification(artificial_scarcity_scaffold, scaffold,
        context(agent_power(organized), _, _, _)).

test(threshold_validation) :-
    % Ensure base extraction meets high-extraction (>= 0.46) logic gates.
    domain_priors:base_extractiveness(artificial_scarcity_scaffold, E),
    E >= 0.46.

:- end_tests(artificial_scarcity_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a "tough medicine" scenario. The base extractiveness (0.48)
 * is high, reflecting significant costs imposed on consumers of the legacy resource.
 * This high extraction, combined with high suppression (0.75), creates a clear
 * Snare from the perspective of a trapped, powerless consumer.
 *
 * However, the system is designed with a clear sunset clause and a coordination
 * function (benefiting future users of the new system). This allows the Architect
 * perspective to classify it as a Scaffold. The high base extraction is unusual
 * for a Scaffold (typically <= 0.30) but is tolerated here precisely because of
 * the sunset clause and the severity of the coordination problem being solved.
 *
 * * PERSPECTIVAL GAP:
 * The gap is stark: The Consumer experiences a Snare due to immediate financial
 * and choice-based friction. The Architect sees a Scaffold—a temporary, necessary
 * structure that supports the "bridge" to abundance. The Institutional beneficiary
 * (the new system) perceives negative extraction (a Rope), as it is the recipient
 * of the coordinated transition.
 *
 * * [RESOLVED MANDATROPHY]:
 * The classification of 'Scaffold' prevents the system from mislabeling
 * this high-suppression state as pure, permanent extraction (Snare). It acknowledges
 * the temporary nature and coordination goal, even with high friction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_scarcity_sunset,
    'Will the scarcity expire as scheduled, or will the "Scaffold" become a permanent "Snare" or "Piton"?',
    'Auditing transition milestones against the hard-coded sunset timestamp and monitoring for attempts to extend the transition period.',
    'If sunset honored: Transition succeeds, constraint dissolves. If delayed: Degrades to Snare as temporality proves false.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(artificial_scarcity_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Required for high-extraction constraints.
% Theater ratio over time: Modeling a functional start (0.10) drifting toward
% the current base management (0.20) as the scaffold matures.
narrative_ontology:measurement(scarcity_tr_t0, artificial_scarcity_scaffold, theater_ratio, 0, 0.10).
narrative_ontology:measurement(scarcity_tr_t5, artificial_scarcity_scaffold, theater_ratio, 5, 0.15).
narrative_ontology:measurement(scarcity_tr_t10, artificial_scarcity_scaffold, theater_ratio, 10, 0.20).

% Extraction over time: Tracking the ramp-up of resource redirection/fees
% during the 10-year migration interval.
narrative_ontology:measurement(scarcity_ex_t0, artificial_scarcity_scaffold, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(scarcity_ex_t5, artificial_scarcity_scaffold, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(scarcity_ex_t10, artificial_scarcity_scaffold, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a classic resource allocation problem: moving users from one
% resource pool to another.
narrative_ontology:coordination_type(artificial_scarcity_scaffold, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */