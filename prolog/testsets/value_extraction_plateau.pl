% ============================================================================
% CONSTRAINT STORY: value_extraction_plateau
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_value_extraction_plateau, []).

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
 * * constraint_id: value_extraction_plateau
 * human_readable: The Law of Diminishing Predation
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where a dominant platform or institution has reached the biological
 * or economic limit of how much surplus it can siphon from its subjects.
 * To maintain growth, the institution enters a state of hyper-theater,
 * pretending to innovate while actually cannibalizing its own infrastructure.
 * This functions as a Snare for users with nowhere else to go and a Piton
 * for the system's own long-term viability.
 * * KEY AGENTS:
 * - Saturated User: Subject (Powerless)
 * - Legacy Platform: Beneficiary (Institutional)
 * - Post-Growth Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(value_extraction_plateau, 0.89). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(value_extraction_plateau, 0.78).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(value_extraction_plateau, 0.94).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(value_extraction_plateau, extractiveness, 0.89).
narrative_ontology:constraint_metric(value_extraction_plateau, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(value_extraction_plateau, theater_ratio, 0.94).

% Constraint self-claim (what does the constraint claim to be?)
% The platform claims to be providing essential coordination infrastructure.
narrative_ontology:constraint_claim(value_extraction_plateau, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(value_extraction_plateau). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(value_extraction_plateau, legacy_platform).
narrative_ontology:constraint_victim(value_extraction_plateau, saturated_user).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: the service they rely on is getting more expensive
% and lower quality, but exit costs (network effects) are too high.
constraint_indexing:constraint_classification(value_extraction_plateau, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the plateau as a Rope—the only way to coordinate
% shareholder expectations and maintain market order through "optimization."
constraint_indexing:constraint_classification(value_extraction_plateau, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Detects high extraction (0.89), high suppression (0.78),
% a coordination function (beneficiary exists), asymmetric extraction (victim exists),
% and active enforcement, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(value_extraction_plateau, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE FUNCTIONAL AUDITOR (PITON)
% A different analytical lens focusing on functional utility vs. performative action.
% The extreme theater ratio (0.94 > 0.70) triggers a Piton classification.
constraint_indexing:constraint_classification(value_extraction_plateau, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(value_extraction_plateau, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(value_extraction_plateau_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(value_extraction_plateau, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(value_extraction_plateau, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify the base extraction is high enough to trigger high-extraction rules.
    narrative_ontology:constraint_metric(value_extraction_plateau, extractiveness, E),
    E >= 0.46.

test(tangled_rope_conditions_met) :-
    % Verify that the analytical observer correctly classifies this as a tangled_rope.
    constraint_indexing:constraint_classification(value_extraction_plateau, tangled_rope,
        context(agent_power(analytical), time_horizon(civilizational), _, _)).

:- end_tests(value_extraction_plateau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.89) reflects the 'Mandatrophy' threshold where
 * the institution's need for growth has essentially eaten the coordinate
 * benefit it once provided. The high theater ratio (0.94) shows that this
 * decay is masked by performative "innovation" narratives. The addition of
 * `requires_active_enforcement` is critical for the Tangled Rope classification,
 * representing the platform's use of lock-in mechanisms to prevent user exit.
 *
 * * PERSPECTIVAL GAP:
 * The Saturated User feels a Snare because they are being squeezed by
 * "anti-features" and price hikes. The Legacy Platform sees a Rope
 * because aggressive extraction is the only coordination signal
 * left to satisfy its capital structure. The Analytical Observer sees a
 * Tangled Rope, recognizing both the coordination claim and the coercive
 * extraction. A Functional Auditor sees a Piton, concluding the original
 * function is almost entirely gone, replaced by theater.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. This identifies
 * that the "coordination" is now a performative facade (Theater 0.94)
 * that masks the terminal extraction of the system's remaining value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_value_extraction_plateau,
    'Can the plateau be breached by genuine innovation, or is the limit biological (Snare vs Mountain)?',
    'Tracking the delta between R&D spend and actual realized utility for the user.',
    'If utility rises: Snare of current management. If utility stays flat: Mountain of Satiety.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(value_extraction_plateau, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(plateau_tr_t0, value_extraction_plateau, theater_ratio, 0, 0.15).
narrative_ontology:measurement(plateau_tr_t5, value_extraction_plateau, theater_ratio, 5, 0.52).
narrative_ontology:measurement(plateau_tr_t10, value_extraction_plateau, theater_ratio, 10, 0.94).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(plateau_ex_t0, value_extraction_plateau, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(plateau_ex_t5, value_extraction_plateau, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(plateau_ex_t10, value_extraction_plateau, base_extractiveness, 10, 0.89).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The platform provides a service that functions as a piece of global infrastructure.
narrative_ontology:coordination_type(value_extraction_plateau, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */