% ============================================================================
% CONSTRAINT STORY: planning_fallacy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_planning_fallacy, []).

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
 * * constraint_id: planning_fallacy
 * human_readable: The Planning Fallacy
 * domain: economic/social/technological
 * * SUMMARY:
 * The planning fallacy is a cognitive bias where predictions about the time needed to complete a task exhibit an optimism bias, underestimating the actual time required. This occurs even when planners know similar past tasks took longer. The constraint is the institutional exploitation of this bias to extract labor and justify resource allocation.
 * * KEY AGENTS:
 * - Individual Contributor: Subject (Powerless), forced to absorb time debt via crunch.
 * - Project Manager/Founder: Beneficiary (Institutional), uses optimistic timelines to secure funding and motivate teams.
 * - Cognitive Scientist: Observer (Analytical), views the bias as a fundamental feature of human cognition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(planning_fallacy, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(planning_fallacy, 0.60).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(planning_fallacy, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(planning_fallacy, extractiveness, 0.55).
narrative_ontology:constraint_metric(planning_fallacy, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(planning_fallacy, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(planning_fallacy, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(planning_fallacy). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(planning_fallacy, institutional_planners).
narrative_ontology:constraint_beneficiary(planning_fallacy, capital_allocators).
narrative_ontology:constraint_victim(planning_fallacy, individual_contributors).
narrative_ontology:constraint_victim(planning_fallacy, project_laborers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL CONTRIBUTOR (SNARE)
% The initial underestimation creates an impossible deadline, extracting unpaid
% overtime ("crunch") to compensate.
constraint_indexing:constraint_classification(planning_fallacy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PROJECT MANAGER / FOUNDER (ROPE)
% Viewed as a necessary tool for optimism and coordination. Without it, the
% daunting reality of a project's true cost might prevent it from ever starting.
constraint_indexing:constraint_classification(planning_fallacy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE COGNITIVE SCIENTIST (MOUNTAIN)
% To the scientist, the underlying bias is a Mountain—an apparently unchangeable
% feature of human cognition (the "Inside View") that favors specific plans
% over statistical base rates.
constraint_indexing:constraint_classification(planning_fallacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical context reveals a hybrid structure. It has a genuine
% coordination function (getting projects started) but also enables asymmetric
% extraction (labor exploitation) and requires active enforcement (suppressing
% realistic forecasts).
constraint_indexing:constraint_classification(planning_fallacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planning_fallacy_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(planning_fallacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planning_fallacy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(planning_fallacy, ExtMetricName, E),
    (E =< 0.15 -> fail ; E >= 0.46). % Ensures it's a high-extraction constraint.

test(tangled_rope_structural_properties) :-
    % Verify the analytical observer sees a Tangled Rope due to the correct structural properties.
    constraint_indexing:constraint_classification(planning_fallacy, tangled_rope, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_beneficiary(planning_fallacy, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(planning_fallacy, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(planning_fallacy).

:- end_tests(planning_fallacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The planning fallacy is a classic example of a multi-type constraint. Its classification depends entirely on the agent's relationship to the flow of time and labor.
 * - Base Extractiveness (0.55): Set above the Snare threshold (0.46) because the institutionalization of this bias systematically extracts "buffer time" and personal well-being from laborers to meet unrealistic deadlines.
 * - Suppression Score (0.60): High because overcoming the fallacy requires using "Reference Class Forecasting" (looking at historical data), a practice often actively suppressed in organizations that value optimism and speed over accuracy.
 * - Perspectival Gap: The core conflict is between the manager who uses the fallacy as a Rope to coordinate optimistic action and the laborer who experiences the consequences as a Snare of inescapable crunch time. The cognitive scientist sees the underlying bias as a Mountain, a fixed feature of human psychology, creating a three-way perspectival split.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * A naive analysis might label this a pure Snare (deceptive schedules) or a pure Mountain (it's just how brains work). The Tangled Rope classification from the analytical perspective is crucial for resolving this. It correctly identifies that the constraint has a genuine coordination function (the Rope aspect) that is inextricably linked to its extractive function (the Snare aspect). This prevents the system from mislabeling the coordination as pure extraction or ignoring the extraction as a natural consequence of a cognitive limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_planning_fallacy,
    "To what degree is the fallacy an unconscious cognitive bias (Mountain) versus a deliberate, strategic tool for securing funding and compelling labor (Snare)?",
    "Analysis of internal vs. external project timeline communications; comparing private estimates with public funding requests across a large dataset of projects.",
    "If primarily strategic, it's a pure Snare of deception. If primarily cognitive, it's a Mountain whose effects are captured by a Snare-like system.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(planning_fallacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Here, we model the fallacy moving
% from a simple cognitive bias to an institutionalized tool for extraction.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (remains low; the fallacy is functional, not performative)
narrative_ontology:measurement(planning_fallacy_tr_t0, planning_fallacy, theater_ratio, 0, 0.10).
narrative_ontology:measurement(planning_fallacy_tr_t5, planning_fallacy, theater_ratio, 5, 0.10).
narrative_ontology:measurement(planning_fallacy_tr_t10, planning_fallacy, theater_ratio, 10, 0.10).

% Extraction over time (increases as the bias is weaponized by management systems)
narrative_ontology:measurement(planning_fallacy_ex_t0, planning_fallacy, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(planning_fallacy_ex_t5, planning_fallacy, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(planning_fallacy_ex_t10, planning_fallacy, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The fallacy is used to coordinate the allocation of capital and labor towards
% ambitious goals that might otherwise seem too costly.
narrative_ontology:coordination_type(planning_fallacy, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */