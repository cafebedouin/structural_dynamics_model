% ============================================================================
% CONSTRAINT STORY: rotation_seven_kubo_ranking
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_rotation_seven_kubo_ranking, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rotation_seven_kubo_ranking
 * human_readable: R7 Kubo Credit and Ranking System
 * domain: economic/social
 * * SUMMARY:
 * The Kubo system is a gamified labor-extraction mechanism on a generational starship. It presents a "ladder" of advancement to children, rewarding agricultural output with credits. While it appears as an unchangeable reality to the children (a False Mountain), it is an institutionally maintained system designed to extract maximum labor while suppressing awareness of the severe health costs (e.g., kidney failure from overwork).
 * * KEY AGENTS:
 * - Station Children (e.g., Rina, Anna): Subjects (Powerless)
 * - Supervisor Kwan: Beneficiary/Manager (Institutional)
 * - Station Command: Beneficiary (Institutional)
 * - Systems Auditor: Observer (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high: essential agricultural labor and biological health are exchanged for non-essential credits.
domain_priors:base_extractiveness(rotation_seven_kubo_ranking, 0.75).
% Suppression is extreme: total enclosure, no memory of Earth, alternatives are non-existent in the conceptual field of the children.
domain_priors:suppression_score(rotation_seven_kubo_ranking, 0.90).
% Theater is low: the system is brutally functional, not performative.
domain_priors:theater_ratio(rotation_seven_kubo_ranking, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, extractiveness, 0.75).
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It presents itself as an immutable law of their world to suppress dissent.
narrative_ontology:constraint_claim(rotation_seven_kubo_ranking, tangled_rope).

% Binary flags
% Enforced via wristbands, medical isolation, and supervisor authority.
domain_priors:requires_active_enforcement(rotation_seven_kubo_ranking).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(rotation_seven_kubo_ranking, station_command).
narrative_ontology:constraint_victim(rotation_seven_kubo_ranking, station_children).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the children, the system is a trap. Their health and well-being are
% extracted in a gamified system where the true costs are hidden.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For Supervisor Kwan, the system is a functional tool (Rope) to manage
% labor, ensure agricultural output, and maintain order. He can adjust
% rankings, using the system to coordinate the workforce effectively.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, the system is a Tangled Rope. It has a genuine coordination
% function (managing labor for station survival) but achieves this through
% extreme asymmetric extraction from a captive population. It requires active
% enforcement to maintain. This classification captures both the Rope-like
% function and the Snare-like harm.
constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotation_seven_kubo_ranking_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify this is correctly identified as a high-extraction constraint.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rotation_seven_kubo_ranking, ExtMetricName, E),
    E >= 0.46.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(rotation_seven_kubo_ranking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores (E=0.75, S=0.90) reflect a system of totalizing exploitation.
 * The perspectival gap is stark: for the children, it's a 'Snare' they cannot escape.
 * For the managers, it's a 'Rope' for coordinating essential labor.
 * The analytical classification must be 'Tangled Rope'. A 'Mountain' is impossible
 * given the high extraction and active enforcement. A pure 'Snare' would miss the
 * genuine (if coercive) coordination function that benefits the station's command
 * structure. Tangled Rope correctly identifies this hybrid nature.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This is a classic Mandatrophic constraint. The system avoids mislabeling it as
 * pure extraction (Snare) by using the Tangled Rope classification from an analytical
 * perspective. This acknowledges the system's functional coordination role for the
 * beneficiaries while fully accounting for the severe, asymmetric extraction
 * imposed on the victims. The constraint's claim to be 'natural_law' is the
 * ideological layer that enables this extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_kubo_system_collapse,
    'What is the critical failure point of the Kubo system: psychological rebellion, biological collapse from overwork, or technological failure?',
    'Long-term monitoring of health data, social unrest metrics, and system-wide agricultural output within the R7 station.',
    'If rebellion: the Snare is cut by victims. If biological collapse: the Snare strangles the entire station. If technological failure: the Rope of control frays.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rotation_seven_kubo_ranking, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the system becoming more brutally efficient over time,
% shedding any pretense (theater) for pure extraction.
%
% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(r7k_tr_t0, rotation_seven_kubo_ranking, theater_ratio, 0, 0.20).
narrative_ontology:measurement(r7k_tr_t5, rotation_seven_kubo_ranking, theater_ratio, 5, 0.15).
narrative_ontology:measurement(r7k_tr_t10, rotation_seven_kubo_ranking, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(r7k_ex_t0, rotation_seven_kubo_ranking, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(r7k_ex_t5, rotation_seven_kubo_ranking, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(r7k_ex_t10, rotation_seven_kubo_ranking, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system allocates labor and rewards, making it a resource allocation mechanism.
narrative_ontology:coordination_type(rotation_seven_kubo_ranking, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */