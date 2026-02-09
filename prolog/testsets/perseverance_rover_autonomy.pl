% ============================================================================
% CONSTRAINT STORY: perseverance_rover_autonomy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_perseverance_rover_autonomy, []).

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
 * * constraint_id: perseverance_rover_autonomy
 * human_readable: Perseverance Rover Autonomy
 * domain: technological
 * * SUMMARY:
 * The Perseverance rover's autonomous exploration capability on Mars presents a constraint on direct, real-time human control. This is a case of delegated decision-making to an AI agent, necessitated by communication latency, which limits the immediate intervention capabilities of Earth-based scientists but vastly increases mission efficiency.
 * * KEY AGENTS:
 * - The Rover: Subject (Powerless) - acting on its programming within physical and mission limits.
 * - NASA Scientists: Beneficiary (Institutional) - enabling more efficient and extensive exploration.
 * - Mars Environment: Analytical Context - the physical reality in which the autonomy operates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(perseverance_rover_autonomy, 0.10). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(perseverance_rover_autonomy, 0.15).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(perseverance_rover_autonomy, 0.05).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(perseverance_rover_autonomy, extractiveness, 0.10).
narrative_ontology:constraint_metric(perseverance_rover_autonomy, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(perseverance_rover_autonomy, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(perseverance_rover_autonomy, rope).

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(perseverance_rover_autonomy, nasa_scientists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% This is a uniform-type constraint (Rope-only). The classification is the
% same from all perspectives because the system is a pure coordination
% mechanism with negligible asymmetric extraction.

% PERSPECTIVE 1: THE SUBJECT (THE ROVER)
% The rover's programming is a constraint, but one that enables its function.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (NASA)
% Viewed as essential infrastructure for mission success.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The system is seen as a pure coordination solution to the problem of
% light-speed delay in interplanetary communication.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perseverance_rover_autonomy_tests).

test(perspectival_consistency) :-
    % Verify there is no perspectival gap because this is a uniform-type Rope.
    constraint_indexing:constraint_classification(perseverance_rover_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perseverance_rover_autonomy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == TypeInstitutional,
    TypePowerless == rope.

test(threshold_validation) :-
    % Verify the base extractiveness is within the Rope threshold.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(perseverance_rover_autonomy, ExtMetricName, E),
    E =< 0.15.

test(theater_ratio_validation) :-
    % Verify the theater ratio is low, indicating high functionality.
    config:param(theater_metric_name, TheaterMetricName),
    narrative_ontology:constraint_metric(perseverance_rover_autonomy, TheaterMetricName, TR),
    TR < 0.70.

:- end_tests(perseverance_rover_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Perseverance rover's autonomy is classified as a pure Rope. The base extractiveness is set very low (0.10) because its primary function is to enable scientific discovery, not to extract value from an agent for the benefit of another. The "extraction" is the loss of direct, real-time control by scientists, but this is a necessary trade-off for operating under significant communication latency and is vastly outweighed by the coordination benefit: enabling autonomous, efficient exploration. The suppression score (0.15) is also low; while it constrains direct control, it opens up far greater possibilities for data collection.
 *
 * This is a uniform-type constraint, classifying as a Rope from all perspectives. For the rover ("powerless"), the autonomy is its mode of function. For NASA ("institutional"), it is the core enabling technology of the mission. There is no victim group, so it cannot be a Tangled Rope or Snare. The theater ratio (0.05) is minimal, as the system is highly functional and not performative, ruling out a Piton classification.
 *
 * [RESOLVED MANDATROPHY]: This is not a high-extraction constraint, so Mandatrophy is not a concern. The Rope classification correctly identifies the system as a coordination solution to a physical problem (light-speed delay), preventing mischaracterization as an extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_perseverance_rover_autonomy,
    'Could future, more advanced AI autonomy on interplanetary probes create unforeseen ethical dilemmas or catastrophic failure modes not present in this system?',
    'Long-term monitoring of AI decision-making, comparative analysis with future autonomous missions, and formal verification of safety-critical AI systems.',
    'If true: The "Rope" of today could become the seed of a future "Snare" if AI goals diverge from human scientific goals. If false: Advanced autonomy remains a robust coordination tool for science.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(perseverance_rover_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a low-extraction constraint, so temporal data is not strictly
% required. However, it is included to model the system's stability over its
% operational interval. Both extraction and theater ratio are expected to
% remain low and stable.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(perseverance_rover_autonomy_tr_t0, perseverance_rover_autonomy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(perseverance_rover_autonomy_tr_t5, perseverance_rover_autonomy, theater_ratio, 5, 0.05).
narrative_ontology:measurement(perseverance_rover_autonomy_tr_t10, perseverance_rover_autonomy, theater_ratio, 10, 0.05).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(perseverance_rover_autonomy_ex_t0, perseverance_rover_autonomy, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(perseverance_rover_autonomy_ex_t5, perseverance_rover_autonomy, base_extractiveness, 5, 0.10).
narrative_ontology:measurement(perseverance_rover_autonomy_ex_t10, perseverance_rover_autonomy, base_extractiveness, 10, 0.10).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system is a unique piece of scientific infrastructure.
narrative_ontology:coordination_type(perseverance_rover_autonomy, global_infrastructure).

% Network relationships (structural influence edges)
% The rover's autonomy is dependent on the communication infrastructure.
narrative_ontology:affects_constraint(deep_space_network_bandwidth, perseverance_rover_autonomy).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (perseverance_rover_autonomy)
% ============================================================================
narrative_ontology:constraint_victim(perseverance_rover_autonomy, none).
