% ============================================================================
% CONSTRAINT STORY: social_credit_architecture
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_social_credit_architecture, []).

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
 * * constraint_id: social_credit_architecture
 * human_readable: Social Credit Architecture
 * domain: social/political/technological
 * * SUMMARY:
 * A national-scale system that operationalizes "trustworthiness" by decoding
 * behavioral data into a unified score. It creates real-world consequences
 * (e.g., access to travel, loans, employment) based on compliance with
 * state-sanctioned norms, blending surveillance with social engineering.
 * * KEY AGENTS:
 * - Low-Score Citizens: Subjects (Powerless)
 * - State Planners & High-Score Elites: Beneficiaries (Institutional)
 * - External Policy Analysts: Auditors (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(social_credit_architecture, 0.80). % Snare extraction >= 0.46
domain_priors:suppression_score(social_credit_architecture, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(social_credit_architecture, 0.15).       % Not a Piton.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(social_credit_architecture, extractiveness, 0.80).
narrative_ontology:constraint_metric(social_credit_architecture, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(social_credit_architecture, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% It is presented as a tool for societal coordination and trust-building.
narrative_ontology:constraint_claim(social_credit_architecture, coordination).

% Binary flags
domain_priors:requires_active_enforcement(social_credit_architecture). % Required for Tangled Rope

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(social_credit_architecture, state_planners).
narrative_ontology:constraint_victim(social_credit_architecture, low_score_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a citizen with a low score, the system is a trap that curtails economic
% and social mobility, extracting compliance through punitive measures.
% χ = 0.80 * 1.5 (powerless) * 1.0 (national) = 1.2
constraint_indexing:constraint_classification(social_credit_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the state and high-scoring elites, it's a coordination mechanism that
% reduces social friction, enforces contracts, and promotes stability.
% χ = 0.80 * -0.2 (institutional) * 1.0 (national) = -0.16 (felt as a benefit)
constraint_indexing:constraint_classification(social_credit_architecture, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the coordination function and the severe, asymmetric
% extraction. It is a hybrid system, not a pure snare or pure rope.
% χ = 0.80 * 1.15 (analytical) * 1.2 (global) = 1.104
constraint_indexing:constraint_classification(social_credit_architecture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_credit_architecture_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless (Snare) and institutional (Rope) views.
    constraint_indexing:constraint_classification(social_credit_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_credit_architecture, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must see the hybrid nature of the constraint.
    constraint_indexing:constraint_classification(social_credit_architecture, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeAnalytical == tangled_rope.

test(tangled_rope_structural_requirements_met) :-
    % Verify all three structural properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(social_credit_architecture, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(social_credit_architecture, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(social_credit_architecture).

:- end_tests(social_credit_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to 0.80 to reflect the profound impact on life
 * chances for those who fall afoul of the system. It extracts social and
 * political compliance. The suppression score of 0.70 reflects the difficulty
 * of opting out in a highly digitized society.
 *
 * The Perspectival Gap is stark:
 * - The 'powerless' victim experiences it as a Snare, a coercive trap with no escape.
 * - The 'institutional' beneficiary views it as a Rope, a valuable tool for
 *   maintaining social order and predictability.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED]
 * The system's claim to be a pure coordination tool ('rope') is a form of
 * mandatrophy. The analytical classification as 'tangled_rope' resolves this
 * by acknowledging the genuine coordination function (for beneficiaries) while
 * simultaneously accounting for the severe, asymmetric extraction imposed on
 * victims. This prevents the system from misclassifying it as a pure Snare
 * (ignoring its coordination role) or a pure Rope (ignoring its coercive nature).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% This omega_variable fact resolves the UNRESOLVED_MANDATROPHY linter error.
omega_variable(
    omega_social_credit_architecture,
    'Is the system primarily for economic enforcement (e.g., debt) or for political repression?',
    'Analysis of the ratio of financial defaulters vs. political dissenters on blacklists over a 5-year period.',
    'If primarily economic, it functions closer to a harsh credit system (Tangled Rope). If primarily political, it is a pure instrument of state control (Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(social_credit_architecture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (0.80 > 0.46) shows its
% gradual intensification from a pilot program to a full-scale architecture.
%
% Theater ratio over time (subtle increase in performative compliance):
narrative_ontology:measurement(sca_tr_t0, social_credit_architecture, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sca_tr_t5, social_credit_architecture, theater_ratio, 5, 0.10).
narrative_ontology:measurement(sca_tr_t10, social_credit_architecture, theater_ratio, 10, 0.15).

% Extraction over time (scope creep and intensification):
narrative_ontology:measurement(sca_ex_t0, social_credit_architecture, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(sca_ex_t5, social_credit_architecture, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(sca_ex_t10, social_credit_architecture, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system's primary function is enforcing behavioral norms.
narrative_ontology:coordination_type(social_credit_architecture, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */