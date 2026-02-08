% ============================================================================
% CONSTRAINT STORY: awareness_without_leverage
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_awareness_without_leverage, []).

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
 * * constraint_id: awareness_without_leverage
 * human_readable: The Cassandra Paradox
 * domain: social/political
 * * SUMMARY:
 * This constraint occurs when an agent possesses high-fidelity information
 * regarding a systemic failure but lacks any structural mechanism to
 * influence the outcome. This asymmetry turns the information itself into
 * a psychological Snare rather than a tool for coordination.
 * * KEY AGENTS:
 * - Cassandra: Subject (Powerless)
 * - Inertial System: Beneficiary (Institutional)
 * - Power Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(awareness_without_leverage, 0.73). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(awareness_without_leverage, 0.65).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(awareness_without_leverage, 0.40).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(awareness_without_leverage, extractiveness, 0.73).
narrative_ontology:constraint_metric(awareness_without_leverage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(awareness_without_leverage, theater_ratio, 0.40).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(awareness_without_leverage, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(awareness_without_leverage). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(awareness_without_leverage, inertial_system).
narrative_ontology:constraint_victim(awareness_without_leverage, cassandra).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped by their own knowledge, unable to act or exit.
constraint_indexing:constraint_classification(awareness_without_leverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the distribution of information without leverage as a
% 'Rope'—a way to maintain the appearance of transparency without the risk
% of actual structural disruption.
constraint_indexing:constraint_classification(awareness_without_leverage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% The combination of high extraction, suppression, a coordination function
% (beneficiary), asymmetric extraction (victim), and active enforcement
% classifies this as a Tangled Rope.
constraint_indexing:constraint_classification(awareness_without_leverage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(awareness_without_leverage_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(awareness_without_leverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(awareness_without_leverage, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(awareness_without_leverage, ExtMetricName, E),
    (E =< 0.15 ; E >= 0.46). % Ensures it's either a low-extraction type or high-extraction Snare/Tangled.

:- end_tests(awareness_without_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high extraction score (0.73) reflects the psychological cost imposed on
 * those who are aware of a problem but are structurally prevented from
 * addressing it. The suppression score (0.65) represents the systemic inertia
 * and lack of channels for effective action.
 * * PERSPECTIVAL GAP:
 * The Subject (Cassandra) experiences a Snare: knowledge is a trap, causing
 * distress without providing agency. The Beneficiary (Inertial System)
 * experiences a Rope: the appearance of transparency and information sharing
 * acts as a stabilizing coordination mechanism, placating dissent without
 * ceding control.
 * * [RESOLVED MANDATROPHY]:
 * The Analytical Observer's classification as a 'Tangled Rope' resolves the
 * Mandatrophy. It correctly identifies that the system has a genuine
 * coordination function (information distribution) but that this function is
 * coupled with severe, asymmetric extraction (the psychological burden on the
 * powerless) and requires active enforcement (suppressing alternative channels
 * of influence) to maintain itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_awareness_without_leverage,
    'Is the lack of leverage a design feature (Snare) or a scaling limit (Mountain)?',
    'Implementation of decentralized quadratic voting for informed cohorts.',
    'If leverage increases: Snare of architecture. If stalemate remains: Mountain of complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(awareness_without_leverage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time: Modeling the drift of performative transparency (0.15)
% toward a stable but non-functional state (0.40).
narrative_ontology:measurement(awl_tr_t0, awareness_without_leverage, theater_ratio, 0, 0.15).
narrative_ontology:measurement(awl_tr_t5, awareness_without_leverage, theater_ratio, 5, 0.28).
narrative_ontology:measurement(awl_tr_t10, awareness_without_leverage, theater_ratio, 10, 0.40).

% Extraction over time: Progressive accumulation of the "psychological tax" as
% awareness of systemic failure grows while leverage remains suppressed.
narrative_ontology:measurement(awl_ex_t0, awareness_without_leverage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(awl_ex_t5, awareness_without_leverage, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(awl_ex_t10, awareness_without_leverage, base_extractiveness, 10, 0.73).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint's coordination function is the distribution of information.
narrative_ontology:coordination_type(awareness_without_leverage, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */