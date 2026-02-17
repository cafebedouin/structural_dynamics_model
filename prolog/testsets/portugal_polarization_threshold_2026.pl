% ============================================================================
% CONSTRAINT STORY: portugal_polarization_threshold_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-09
% ============================================================================

:- module(constraint_portugal_polarization_threshold_2026, []).

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
 * * constraint_id: portugal_polarization_threshold_2026
 * human_readable: The "Cordon Sanitaire" / Polarization Threshold
 * domain: political/social
 * * SUMMARY:
 * As the 2026 Presidential election approaches, the "Polarization Threshold"
 * between the right-wing populist party Chega and traditional parties (PSD, PS)
 * functions as a Tangled Rope. While it coordinates the "democratic defense"
 * of institutional norms, it simultaneously extracts political agency by
 * forcing voters into binary choices and suppressing the viability of
 * compromise candidates.
 * * KEY AGENTS:
 * - The Anti-Establishment Voter: Subject (Powerless) - Constrained by the tactical necessity of preventing "extremism" vs "stagnation."
 * - Traditional Parties (PS/PSD): Beneficiary (Institutional) - Benefit from the coordination that solidifies their position as the only viable governing options.
 * - The Systems Auditor: Observer (Analytical) - Analyzes the dual function of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(portugal_polarization_threshold_2026, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(portugal_polarization_threshold_2026, 0.60).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(portugal_polarization_threshold_2026, 0.40).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, theater_ratio, 0.40).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(portugal_polarization_threshold_2026, tangled_rope).
narrative_ontology:human_readable(portugal_polarization_threshold_2026, "The \"Cordon Sanitaire\" / Polarization Threshold").

% Binary flags
domain_priors:requires_active_enforcement(portugal_polarization_threshold_2026). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(portugal_polarization_threshold_2026, traditional_parties).
narrative_ontology:constraint_victim(portugal_polarization_threshold_2026, anti_establishment_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE ANTI-ESTABLISHMENT VOTER (SNARE)
% Experienced as a trap designed to delegitimize dissent and fix the outcome.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL LOYALIST (ROPE)
% Viewed as an essential coordination mechanism to maintain regime stability.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the dual-function of protecting norms while extracting voter choice.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portugal_polarization_threshold_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_observer_sees_tangled_rope) :-
    % Verify the analytical auditor detects the hybrid signature.
    constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(portugal_polarization_threshold_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Polarization Threshold is a textbook Tangled Rope. It provides high
 * coordination value for the "establishment" by defining the boundaries of
 * acceptable governance (beneficiary: traditional_parties), but it is highly
 * extractive for those outside that boundary (victim: anti_establishment_voters)
 * by limiting their political agency. The high suppression score (0.60) reflects
 * how alternative centrist or compromise paths are squeezed by the two poles.
 * The active enforcement is the continuous political and media discourse that
 * reinforces the "cordon sanitaire".
 *
 * * MANDATROPHY ANALYSIS:
 * The base extraction of 0.55 is high, risking a misclassification as a pure Snare.
 * The Tangled Rope classification correctly resolves this by acknowledging the
 * genuine coordination function (defending institutional norms, as claimed by
 * beneficiaries) while simultaneously accounting for the asymmetric extraction
 * imposed on a specific group (voters whose choices are constrained). This
 * prevents the system from ignoring the real, albeit contested, coordination value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_portugal_polarization_threshold_2026,
    'Can an independent candidate untangle the polarization rope, or will they be forced into one of the two poles?',
    'Analyzing the runoff (2nd round) vote transfers in the January 2026 presidential election.',
    'Rupture (Tangled Rope breaks, new political alignment) vs Absorption (Tangled Rope persists, candidate co-opted)',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. The interval 0-10 maps to 2019-2026.
narrative_ontology:interval(portugal_polarization_threshold_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models the intensification of
% polarization in Portugal following the 2019 election where Chega first
% gained a parliamentary seat.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (the "cordon sanitaire" becomes more of a campaign tool):
narrative_ontology:measurement(ppt2026_tr_t0, portugal_polarization_threshold_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ppt2026_tr_t5, portugal_polarization_threshold_2026, theater_ratio, 5, 0.30).
narrative_ontology:measurement(ppt2026_tr_t10, portugal_polarization_threshold_2026, theater_ratio, 10, 0.40).

% Extraction over time (voter choice becomes more constrained as Chega grows):
narrative_ontology:measurement(ppt2026_ex_t0, portugal_polarization_threshold_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ppt2026_ex_t5, portugal_polarization_threshold_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ppt2026_ex_t10, portugal_polarization_threshold_2026, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% A "cordon sanitaire" is a political agreement to exclude a party, functioning
% as a mechanism to enforce a particular political order.
narrative_ontology:coordination_type(portugal_polarization_threshold_2026, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(portugal_polarization_threshold_2026, 0.0).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(portugal_polarization_threshold_2026, [other_constraint_id]).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */