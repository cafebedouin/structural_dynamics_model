% ============================================================================
% CONSTRAINT STORY: perseverance_ai_drive
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_perseverance_ai_drive, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: perseverance_ai_drive
 * human_readable: AI-Driven Martian Rover Autonomy
 * domain: technological
 * * SUMMARY:
 * NASA's Perseverance rover successfully completed its first AI-planned drive on Mars, autonomously navigating a 200-foot (61-meter) path. The constraint lies in the rover's limited resources (processing power, energy) for navigation and decision-making, requiring a balance between autonomy and reliance on Earth-based commands.
 * * KEY AGENTS:
 * - Rover Resources: Subject (Powerless)
 * - NASA Engineers: Beneficiary (Institutional)
 * - Scientific Community: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(perseverance_ai_drive, 0.25). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(perseverance_ai_drive, 0.10).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(perseverance_ai_drive, 0.05).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(perseverance_ai_drive, extractiveness, 0.25).
narrative_ontology:constraint_metric(perseverance_ai_drive, suppression_requirement, 0.10).
narrative_ontology:constraint_metric(perseverance_ai_drive, theater_ratio, 0.05).

% Constraint classification claim
narrative_ontology:constraint_claim(perseverance_ai_drive, rope).

% Binary flags
% narrative_ontology:has_sunset_clause(perseverance_ai_drive).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(perseverance_ai_drive). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(perseverance_ai_drive, nasa_engineers).
% narrative_ontology:constraint_victim(perseverance_ai_drive, rover_resources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE/MOUNTAIN)
% High extraction felt as an immutable limit or predatory trap.
% NOTE: This may be upgraded to 'organized' power if a critical mass of victims exists.
constraint_indexing:constraint_classification(perseverance_ai_drive, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(perseverance_ai_drive, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perseverance_ai_drive_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(perseverance_ai_drive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perseverance_ai_drive, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(perseverance_ai_drive, ExtMetricName, E),
    E =< 0.15. % Ensures it's a Rope or Mountain.

:- end_tests(perseverance_ai_drive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The rover's limited resources (power, processing) are viewed as a *Mountain* from its perspective, a fundamental constraint.  NASA engineers, however, view the AI-driven autonomy as a *Rope* that allows the rover to accomplish more science with less direct control, enabling more exploration. The analytical perspective also sees it as a Rope, improving efficiency and exploration capacity. The perspectival gap arises from the differing levels of control and impact.  The rover *experiences* resource limitations; the engineers *leverage* the AI to overcome those limitations.
 * * MANDATROPHY ANALYSIS:
 * The system avoids mislabeling this coordination as pure extraction because there is no explicit victim group declared. While the rover experiences limitations, it is not being actively exploited in the traditional sense. The AI autonomy serves to expand the rover's capabilities, not to extract resources from it against its "will."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_perseverance_ai_drive,
    'How reliable will the AI's autonomous navigation be in the long term?',
    'Accumulation of operational data and fault analysis.',
    'Improved science return vs. increased mission risk and potential for rover damage.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(perseverance_ai_drive, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Model how the constraint intensified or changed across the interval.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
% No measurement required since base_extractiveness < 0.46

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */