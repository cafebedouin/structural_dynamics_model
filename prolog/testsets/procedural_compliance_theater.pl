% ============================================================================
% CONSTRAINT STORY: procedural_compliance_theater
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_procedural_compliance_theater, []).

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
 * * constraint_id: procedural_compliance_theater
 * human_readable: The Checklist Trap
 * domain: institutional
 * * SUMMARY:
 * A condition where adherence to bureaucratic process becomes the primary goal,
 * regardless of whether that process achieves its intended outcome. It
 * transforms a once-functional coordination mechanism into a "Piton" that
 * exists to shield the institution from liability rather than to produce value.
 * * KEY AGENTS:
 * - The Worker: Subject (Powerless) - Completing forms that no one reads.
 * - The Institution/Compliance Officer: Beneficiary (Institutional) - Validated by the volume of completed forms.
 * - The Efficiency Auditor: Auditor (Analytical) - Observing the "Theater Ratio."
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(procedural_compliance_theater, 0.58). % High: Time/labor extracted for zero utility.
domain_priors:suppression_score(procedural_compliance_theater, 0.75).   % High: Compliance is mandatory for employment/legitimacy.
domain_priors:theater_ratio(procedural_compliance_theater, 0.88).       % Extremely High: Diagnostic of a Piton.

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(procedural_compliance_theater, extractiveness, 0.58).
narrative_ontology:constraint_metric(procedural_compliance_theater, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(procedural_compliance_theater, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
% The system claims to be a necessary coordination mechanism for legal/regulatory compliance.
narrative_ontology:constraint_claim(procedural_compliance_theater, piton).
narrative_ontology:human_readable(procedural_compliance_theater, "The Checklist Trap").
narrative_ontology:topic_domain(procedural_compliance_theater, "institutional").

% Binary flags
domain_priors:requires_active_enforcement(procedural_compliance_theater).

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(procedural_compliance_theater, institution).
narrative_ontology:constraint_victim(procedural_compliance_theater, worker).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the worker, the checklist is a Snare—a predatory extraction of cognitive
% bandwidth with no exit.
% χ = 0.58 * 1.5 (powerless) * 0.8 (local) = 0.696
constraint_indexing:constraint_classification(procedural_compliance_theater, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the institution, the theater is a Rope—it provides the "coordination"
% necessary to satisfy external regulators and mitigate legal risk.
% χ = 0.58 * -0.2 (institutional) * 1.0 (national) = -0.116 (felt as a net benefit)
constraint_indexing:constraint_classification(procedural_compliance_theater, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Detection of "Piton" status: high theater_ratio (0.88) identifies the
% constraint as functionally inert but structurally persistent.
constraint_indexing:constraint_classification(procedural_compliance_theater, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(procedural_compliance_theater, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_compliance_theater_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(procedural_compliance_theater, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_compliance_theater, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(piton_threshold_validation) :-
    % Verify the auditor classifies this as a Piton due to theater_ratio > 0.70.
    constraint_indexing:constraint_classification(procedural_compliance_theater, piton, context(agent_power(analytical), _, _, _)).

test(high_extraction_metrics_present) :-
    % High extraction requires victim/beneficiary declarations.
    narrative_ontology:constraint_beneficiary(procedural_compliance_theater, _),
    narrative_ontology:constraint_victim(procedural_compliance_theater, _).

:- end_tests(procedural_compliance_theater_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.58) reflects the "tax" on human attention and labor
 * wasted on non-productive checks. The suppression score (0.75) reflects the
 * mandatory nature of this compliance within an organizational hierarchy.
 * The key diagnostic is the theater_ratio of 0.88, which is the primary
 * signal for a Piton classification from an analytical perspective.
 *
 * PERSPECTIVAL GAP:
 * The worker experiences this as a Snare, a pure extraction of their time and
 * effort for no discernible benefit. The institution, however, perceives it as
 * a Rope, as it successfully coordinates the appearance of diligence, thereby
 * satisfying external auditors and insulating itself from legal liability.
 * This gap is the core pathology of compliance theater.
 *
 * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the "Piton" classification. Unlike a "Mountain"
 * of necessary safety rules (e.g., flight checklists), "Theater" is identified
 * by its lack of actual effectiveness despite high formal compliance. The high
 * theater_ratio allows the system to distinguish between functional, necessary
 * bureaucracy and its degraded, inertial form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_procedural_compliance_theater,
    'Does the theater provide a "Legal Rope" that protects the system from collapse, even if the "Technical Rope" is broken?',
    'Analysis of insurance premiums and legal acquittal rates vs. procedural adherence.',
    'If protection exists: It is a Tangled Rope (providing real coordination value to one party); If no protection: It is a pure Snare/Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(procedural_compliance_theater, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This model shows a once-useful
% procedure degrading over time, with performative aspects (theater) and
% extractive overhead growing as its original function atrophies.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(pct_tr_t0, procedural_compliance_theater, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pct_tr_t5, procedural_compliance_theater, theater_ratio, 5, 0.60).
narrative_ontology:measurement(pct_tr_t10, procedural_compliance_theater, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(pct_ex_t0, procedural_compliance_theater, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(pct_ex_t5, procedural_compliance_theater, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pct_ex_t10, procedural_compliance_theater, base_extractiveness, 10, 0.58).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint is a set of rules for demonstrating compliance.
narrative_ontology:coordination_type(procedural_compliance_theater, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */