% ============================================================================
% CONSTRAINT STORY: litchfield_sensitive_locations_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_litchfield_sensitive_locations_2026, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: litchfield_sensitive_locations_2026
 * human_readable: Litchfield School Perimeter Crisis
 * domain: political/social
 * * SUMMARY:
 * On Feb 5, 2026, federal agents were spotted within a block of the School of
 * St. Philip in Litchfield, MN, despite a publicized 700-agent drawdown.
 * This presence at "sensitive locations" triggers a Snare classification for
 * residents, as it extracts student mental health and attendance to maintain
 * a 2,300-agent enforcement baseline.
 * * KEY AGENTS:
 * - Parents/Students (School of St. Philip): Subject (Powerless)
 * - Federal Executive (Trump Administration): Beneficiary (Institutional)
 * - MN School Districts (Litigants): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, 0.78). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(litchfield_sensitive_locations_2026, 0.94).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(litchfield_sensitive_locations_2026, 0.88).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, extractiveness, 0.78).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, suppression_requirement, 0.94).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(litchfield_sensitive_locations_2026, piton).
narrative_ontology:human_readable(litchfield_sensitive_locations_2026, "Litchfield School Perimeter Crisis").

% Binary flags
domain_priors:requires_active_enforcement(litchfield_sensitive_locations_2026).

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(litchfield_sensitive_locations_2026, federal_enforcement_agencies).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, litchfield_students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Local parents view the school perimeter presence as a Snare: a predatory
% environment that traps children between educational needs and federal threat.
% χ = 0.78 (ε) * 1.5 (π(powerless)) * 0.9 (σ(regional)) = 1.053. This high
% effective extraction confirms the Snare classification.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Administration views the "Metro Surge" as a Rope: essential coordination
% for national security, regardless of local proximity to schools.
% χ = 0.78 (ε) * -0.2 (π(institutional)) * 1.0 (σ(national)) = -0.156. The
% negative effective extraction indicates a perceived benefit, classifying as Rope.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Analysts (School Districts) see a Piton. The "security" function has
% atrophied into theatrical enforcement that damages local stability, confirmed
% by the high theater_ratio.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(litchfield_sensitive_locations_2026, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(litchfield_sensitive_locations_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(extraction_threshold) :-
    domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, E),
    E > 0.46.

test(theater_dominance) :-
    domain_priors:theater_ratio(litchfield_sensitive_locations_2026, TR),
    TR > 0.70.

:- end_tests(litchfield_sensitive_locations_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) reflects the measurable drop in attendance
 * and mental health outcomes for students. The suppression score (0.94) is
 * justified by the presence of federal force in designated "sensitive locations,"
 * effectively removing alternatives for the local community. The Theater Ratio
 * (0.88) is justified by the discrepancy between the "700-agent drawdown"
 * announcement and the tactical reality of agents stationed near the School of
 * St. Philip.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The high extraction (0.78) and suppression (0.94)
 * could lead to a simple Snare classification. However, the system correctly
 * identifies this as a Piton from an analytical perspective due to the
 * extremely high theater_ratio (0.88). This resolves mandatrophy by
 * recognizing that the constraint's original coordination claim has decayed
 * into performative enforcement, preventing a misclassification that would
 * ignore the constraint's history and theatrical nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_litchfield_2026,
    'Will the school district lawsuit establish a legal Mountain against federal entry?',
    'Judicial ruling on federal activity in "sensitive locations".',
    'Success creates a Mountain (limit); Failure reinforces the Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(litchfield_sensitive_locations_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Model how the constraint intensified or changed across the interval.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(litchfield_sensitive_locations_2026_tr_t0, litchfield_sensitive_locations_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(litchfield_sensitive_locations_2026_tr_t5, litchfield_sensitive_locations_2026, theater_ratio, 5, 0.60).
narrative_ontology:measurement(litchfield_sensitive_locations_2026_tr_t10, litchfield_sensitive_locations_2026, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(litchfield_sensitive_locations_2026_ex_t0, litchfield_sensitive_locations_2026, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(litchfield_sensitive_locations_2026_ex_t5, litchfield_sensitive_locations_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(litchfield_sensitive_locations_2026_ex_t10, litchfield_sensitive_locations_2026, base_extractiveness, 10, 0.78).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(litchfield_sensitive_locations_2026, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(litchfield_sensitive_locations_2026, 0.1).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(litchfield_sensitive_locations_2026, local_policing_autonomy).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */