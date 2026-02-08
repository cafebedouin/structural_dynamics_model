% ============================================================================
% CONSTRAINT STORY: paris_municipal_reform_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_paris_municipal_reform_2026, []).

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
 * * constraint_id: paris_municipal_reform_2026
 * human_readable: Paris Municipal Reform (Loi Maillard/PLM Reform)
 * domain: political/legal
 * * SUMMARY:
 * The 2026 reform (Loi du 11 août 2025) fundamentally shifts the "Majority
 * Premium" (Prime Majoritaire) for the Council of Paris from 50% down to 25%
 * of seats. While intended to enhance proportionality and democratic
 * visibility, the transition creates a "Tangled Rope" dynamic where
 * coordination (direct election) is bundled with a new form of institutional
 * extraction: the legislative fragmentation of the "Majority Premium", which
 * extracts governability from the winning party.
 * * KEY AGENTS:
 * - Voter for a minority party: Subject (Powerless) - Gains representation.
 * - Incumbent Majority Party: Victim (Institutional) - Loses guaranteed majority, making governance harder.
 * - Smaller Political Parties: Beneficiary (Organized) - Gain seats and influence.
 * - Legislative Auditor: Auditor (Analytical) - Observes the hybrid nature of the reform.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(paris_municipal_reform_2026, 0.47). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(paris_municipal_reform_2026, 0.35).   % Structural property (raw, unscaled). The reform lowers suppression of minority voices.
domain_priors:theater_ratio(paris_municipal_reform_2026, 0.25).       % Piton detection (>= 0.70). The reform has concrete, non-theatrical effects.

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(paris_municipal_reform_2026, extractiveness, 0.47).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(paris_municipal_reform_2026, theater_ratio, 0.25).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(paris_municipal_reform_2026, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(paris_municipal_reform_2026). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(paris_municipal_reform_2026, smaller_political_parties).
narrative_ontology:constraint_victim(paris_municipal_reform_2026, incumbent_majority_party).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (POWERLESS VOTER)
% For a voter whose party was previously unrepresented, the reform is pure
% coordination, better aligning their vote with council seats.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE VICTIM (INSTITUTIONAL MAJORITY)
% The incumbent party sees the reduction of the majority premium as a Snare that
% extracts governability and makes the city unmanageable without costly coalitions.
constraint_indexing:constraint_classification(paris_municipal_reform_2026, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Detects the hybrid nature: a genuine coordination benefit (for voters)
% coupled with asymmetric extraction (of stability from the winner).
constraint_indexing:constraint_classification(paris_municipal_reform_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_municipal_reform_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(paris_municipal_reform_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paris_municipal_reform_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == rope,
    TypeInstitutional == snare,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must see Tangled Rope due to the hybrid metrics.
    constraint_indexing:constraint_classification(paris_municipal_reform_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(paris_municipal_reform_2026, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(paris_municipal_reform_2026, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(paris_municipal_reform_2026).

:- end_tests(paris_municipal_reform_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Tangled Rope" classification is central here because the reform
 * solves a coordination problem (voter confusion over indirect election and
 * lack of proportionality) but introduces an "extractive" tax on governability.
 * By cutting the Majority Premium in half (50% -> 25%), the law forces the winning
 * party to "pay" in seats to achieve a broader democratic consensus. This creates
 * a sharp perspectival gap: it's a Rope for minority voters (who gain representation)
 * but a Snare for the incumbent majority (who lose stability). The analytical
 * view correctly identifies this hybrid nature as a Tangled Rope.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system remains a Rope for the electorate (higher fidelity) but
 * becomes a Snare for the executive. The "Tangled" classification
 * prevents the system from being mislabeled as pure coordination (Rope) or
 * pure extraction (Snare), capturing the essential trade-off at its core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_paris_municipal_reform_2026,
    'Will the 25% premium be sufficient to form a stable majority in a 5-list runoff?',
    'Empirical results from the March 2026 municipal election and subsequent coalition negotiations.',
    'If insufficient, the reform is a failed Scaffold that created gridlock (Snare). If sufficient, it is a successful Tangled Rope that balanced representation and governance.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(paris_municipal_reform_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Required for high-extraction
% constraints (base_extractiveness > 0.46). This reform introduces the
% extraction at T=0 and it remains constant.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(pmr26_tr_t0, paris_municipal_reform_2026, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pmr26_tr_t5, paris_municipal_reform_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(pmr26_tr_t10, paris_municipal_reform_2026, theater_ratio, 10, 0.25).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(pmr26_ex_t0, paris_municipal_reform_2026, base_extractiveness, 0, 0.47).
narrative_ontology:measurement(pmr26_ex_t5, paris_municipal_reform_2026, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(pmr26_ex_t10, paris_municipal_reform_2026, base_extractiveness, 10, 0.47).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This reform is a mechanism for allocating a resource (council seats).
narrative_ontology:coordination_type(paris_municipal_reform_2026, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */