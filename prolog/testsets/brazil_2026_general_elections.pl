% ============================================================================
% CONSTRAINT STORY: brazil_2026_general_elections
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_brazil_2026_general_elections, []).

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
 * * constraint_id: brazil_2026_general_elections
 * human_readable: 2026 Brazilian General Election Structure
 * domain: political
 * * SUMMARY:
 * The 2026 elections in Brazil operate as a rigid structural constraint defined
 * by the "Bolsonarismo vs. Lulismo" binary. While the incumbent (Lula) seeks
 * re-election, the judicial disqualification of Jair Bolsonaro has forced the
 * Right into a "Piton" state of inertial grievance, while the "Centrão" acts as
 * a Tangled Rope extracting state resources for legislative cooperation.
 * * KEY AGENTS:
 * - The "Polarized Voter": Subject (Powerless)
 * - The "Centrão" Bloc: Beneficiary (Institutional)
 * - STF (Supreme Federal Court): Auditor/Enforcer (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(brazil_2026_general_elections, 0.52). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(brazil_2026_general_elections, 0.42).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(brazil_2026_general_elections, 0.75).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(brazil_2026_general_elections, extractiveness, 0.52).
narrative_ontology:constraint_metric(brazil_2026_general_elections, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(brazil_2026_general_elections, theater_ratio, 0.75).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(brazil_2026_general_elections, tangled_rope).
narrative_ontology:human_readable(brazil_2026_general_elections, "2026 Brazilian General Election Structure").

% Binary flags
domain_priors:requires_active_enforcement(brazil_2026_general_elections). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(brazil_2026_general_elections, centrao_bloc).
narrative_ontology:constraint_victim(brazil_2026_general_elections, brazilian_taxpayer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE DISILLUSIONED VOTER (SNARE)
% High extraction (taxation vs services) felt as a predatory trap with no exit.
constraint_indexing:constraint_classification(brazil_2026_general_elections, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE "CENTRÃO" LEADERSHIP (ROPE)
% Viewed as the essential coordination mechanism for governability.
constraint_indexing:constraint_classification(brazil_2026_general_elections, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE POLITICAL SCIENTIST (TANGLED ROPE)
% Detects the hybrid nature of democratic coordination used for asymmetric extraction.
constraint_indexing:constraint_classification(brazil_2026_general_elections, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE BOLSONARISTA BASE (PITON)
% Maintenance of a movement whose primary functional lead (the candidate) is inert.
constraint_indexing:constraint_classification(brazil_2026_general_elections, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_2026_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(brazil_2026_general_elections, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_2026_general_elections, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(brazil_2026_general_elections, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_2026_general_elections, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(brazil_2026_general_elections, ExtMetricName, E),
    (E =< 0.15 ; E >= 0.46). % Passes v3.4 threshold for either low or high-extraction logic.

test(piton_detection) :-
    config:param(theater_metric_name, TheaterMetricName),
    narrative_ontology:constraint_metric(brazil_2026_general_elections, TheaterMetricName, TR),
    TR >= 0.70,
    constraint_indexing:constraint_classification(brazil_2026_general_elections, piton, _).

:- end_tests(brazil_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.52 extractiveness reflects the "cost of governability" in Brazil,
 * where the executive must cede vast budgetary control (e.g., "secret budget")
 * to the legislative center (Centrão) to pass legislation. The Perspectival
 * Gap is stark: the "Institutional" agents (Centrão) see this as a Rope
 * (coordination), while the "Powerless" taxpayer sees it as a Snare (resource
 * drain without commensurate representation or services). The high theater
 * ratio (0.75) captures the Bolsonarista movement's focus on an ineligible
 * candidate, making their political activity a Piton of inertial grievance.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical here. A simpler model would
 * classify the system as a pure Snare due to its high extraction (E=0.52).
 * However, this misses the genuine coordination function: the Centrão's
 * mechanism, while extractive, does prevent total institutional collapse and
 * legislative gridlock. The Tangled Rope acknowledges this dual nature,
 * preventing the system from mislabeling high-cost coordination as pure
 * predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_brazil_eligibility,
    'Will the Supreme Court (STF) maintain the ineligibility of the opposition leader?',
    'Judicial review of late 2025 appeals.',
    'If False, the Piton classification for the opposition reverts to a functional Rope/Snare; if True, the Snare risk for the Right increases due to lack of viable alternatives.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(brazil_2026_general_elections, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Here, we model the gradual entrenchment of budgetary extraction and the
% rise of political theater as the election cycle progresses.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(brazil_2026_tr_t0, brazil_2026_general_elections, theater_ratio, 0, 0.60).
narrative_ontology:measurement(brazil_2026_tr_t5, brazil_2026_general_elections, theater_ratio, 5, 0.70).
narrative_ontology:measurement(brazil_2026_tr_t10, brazil_2026_general_elections, theater_ratio, 10, 0.75).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(brazil_2026_ex_t0, brazil_2026_general_elections, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(brazil_2026_ex_t5, brazil_2026_general_elections, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(brazil_2026_ex_t10, brazil_2026_general_elections, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The Centrão's function is primarily one of resource allocation (budgetary
% amendments) to achieve legislative coordination.
narrative_ontology:coordination_type(brazil_2026_general_elections, resource_allocation).

% Network relationships (structural influence edges)
% The electoral structure and its associated budget negotiations directly
% impact the country's ability to adhere to fiscal rules.
narrative_ontology:affects_constraint(brazil_2026_general_elections, fiscal_responsibility_law).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */