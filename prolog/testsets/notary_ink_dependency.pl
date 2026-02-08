% ============================================================================
% CONSTRAINT STORY: notary_ink_dependency
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_notary_ink_dependency, []).

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
 * * constraint_id: notary_ink_dependency
 * human_readable: The Notary/Wet-Ink Persistence
 * domain: legal/institutional
 * * SUMMARY:
 * This constraint represents the mandatory requirement for physical
 * presence and manual signatures for high-value legal documents. Despite
 * the existence of superior cryptographic verification, the "Notary"
 * remains a Piton: an inertial anchor used for psychological signaling
 * and institutional risk-mitigation.
 * * KEY AGENTS:
 * - The Signatory: Subject (Powerless). Must travel and pay for a ritual.
 * - The Legal Institution: Beneficiary (Institutional). Relies on the
 * tradition to manage liability and maintain professional gatekeeping.
 * - The Digital Auditor: Auditor (Analytical). Identifies the theater.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(notary_ink_dependency, 0.35). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46. This is mid-range.
domain_priors:suppression_score(notary_ink_dependency, 0.88).   % High: Many documents are legally invalid without this specific ritual.
domain_priors:theater_ratio(notary_ink_dependency, 0.92).       % Piton detection (>= 0.70). Very high.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(notary_ink_dependency, extractiveness, 0.35).
narrative_ontology:constraint_metric(notary_ink_dependency, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(notary_ink_dependency, theater_ratio, 0.92).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(notary_ink_dependency, piton).

% Binary flags
narrative_ontology:has_sunset_clause(notary_ink_dependency).      % Mandatory if Scaffold. Assumed for e-governance transitions.
domain_priors:requires_active_enforcement(notary_ink_dependency). % Required for Tangled Rope. Courts enforce notary requirements.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(notary_ink_dependency, legal_institutions).
narrative_ontology:constraint_victim(notary_ink_dependency, signatories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SIGNATORY (SNARE)
% For the individual, the need to find a physical notary during business
% hours is a Snare—a low-utility friction that restricts movement.
constraint_indexing:constraint_classification(notary_ink_dependency, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BANKING SYSTEM (ROPE)
% Viewed institutionally, the notary is a Rope. It provides a shared
% standard of "High-Trust" that coordinates risk across different jurisdictions.
constraint_indexing:constraint_classification(notary_ink_dependency, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE DIGITAL AUDITOR (PITON)
% Analytically, the notary is a Piton. It is an artifact of the 19th-century
% coordination stack maintained solely by institutional habit.
constraint_indexing:constraint_classification(notary_ink_dependency, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(notary_ink_dependency, TR), TR > 0.70.

% PERSPECTIVE 4: THE E-GOVERNANCE ADVOCATE (SCAFFOLD)
% In jurisdictions transitioning to digital-first, the notary is a
% Scaffold—a bridge to keep the system running until DIDs are universal.
constraint_indexing:constraint_classification(notary_ink_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(notary_ink_dependency).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notary_ink_dependency_tests).

test(piton_detection) :-
    % Ensure the analytical perspective identifies this as a Piton due to theater_ratio.
    constraint_indexing:constraint_classification(notary_ink_dependency, piton, context(agent_power(analytical), _, _, _)).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(notary_ink_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(notary_ink_dependency, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(scaffold_conditions_met) :-
    % Verify scaffold classification is possible because its conditions are met.
    narrative_ontology:has_sunset_clause(notary_ink_dependency),
    narrative_ontology:constraint_beneficiary(notary_ink_dependency, _).

:- end_tests(notary_ink_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Notary Ink Dependency is a high-theater constraint (TR = 0.92).
 * It functions as a Piton because its original "Rope" function (manual
 * verification by a trusted witness) has been made obsolete by digital
 * signatures, yet the institutional structure remains "hooked" into it.
 * The Perspectival Gap exists because institutions confuse the "Ritual
 * of Trust" (Rope) with the "Mechanism of Security," while individuals
 * experience only the coercive friction (Snare). The Scaffold perspective
 * acknowledges its temporary utility in a transitional legal system.
 *
 * MANDATROPHY ANALYSIS:
 * This represents "Inertial Mandatrophy." The coordination function
 * (verifying identity) is still performed, but at a vastly higher cost
 * than available alternatives, purely to preserve institutional tradition.
 * The extraction (0.35) consists of "Friction Costs" (time, travel, fees).
 * It is not predatory, but it is no longer optimized for coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_notary_psychological_deterrence,
    'Does the physical act of "wet ink" presence deter fraud more than digital signatures?',
    'Comparative study of fraud rates in e-notary vs. traditional notary states.',
    'If yes, the Piton is actually a Rope; if no, it is a pure Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(notary_ink_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is not high-extraction, but temporal data is included to model
% its degradation from a functional Rope into a high-theater Piton.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(notary_ink_dependency_tr_t0, notary_ink_dependency, theater_ratio, 0, 0.20).
narrative_ontology:measurement(notary_ink_dependency_tr_t5, notary_ink_dependency, theater_ratio, 5, 0.65).
narrative_ontology:measurement(notary_ink_dependency_tr_t10, notary_ink_dependency, theater_ratio, 10, 0.92).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(notary_ink_dependency_ex_t0, notary_ink_dependency, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(notary_ink_dependency_ex_t5, notary_ink_dependency, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(notary_ink_dependency_ex_t10, notary_ink_dependency, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(notary_ink_dependency, information_standard).

% Network relationships (structural influence edges)
% The persistence of wet-ink requirements structurally slows the adoption of
% more efficient digital identity systems.
narrative_ontology:affects_constraint(notary_ink_dependency, digital_identity_adoption).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */