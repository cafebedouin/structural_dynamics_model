% ============================================================================
% CONSTRAINT STORY: mil_std_461g_emi_control
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_mil_std_461g_emi_control, []).

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
 * * constraint_id: mil_std_461g_emi_control
 * human_readable: MIL-STD-461G EMI Control Standard
 * domain: technological
 * * SUMMARY:
 * MIL-STD-461G establishes interface and verification requirements for controlling
 * electromagnetic interference (EMI) in military subsystems and equipment. It aims
 * to ensure interoperability in complex electromagnetic environments but imposes
 * significant testing and design costs.
 * * KEY AGENTS:
 * - Small Defense Contractor: Subject (Powerless), faces high compliance costs.
 * - DoD Procuring Activity: Beneficiary (Institutional), enforces the standard to ensure mission reliability.
 * - Systems Engineer: Auditor (Analytical), recognizes both the coordination function and the extractive costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(mil_std_461g_emi_control, 0.5). % Mountain <= 0.15, Snare >= 0.46. High cost of specialized test facilities and redesigns.
domain_priors:suppression_score(mil_std_461g_emi_control, 0.4).   % Suppresses cheaper commercial alternatives (e.g., FCC standards) for military applications.
domain_priors:theater_ratio(mil_std_461g_emi_control, 0.1).       % Low; the standard is highly functional, not performative. Piton requires >= 0.70.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(mil_std_461g_emi_control, extractiveness, 0.5).
narrative_ontology:constraint_metric(mil_std_461g_emi_control, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(mil_std_461g_emi_control, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(mil_std_461g_emi_control, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(mil_std_461g_emi_control). % Required for Tangled Rope. Compliance requires active testing and verification.

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(mil_std_461g_emi_control, dod_mission_safety).
narrative_ontology:constraint_beneficiary(mil_std_461g_emi_control, large_defense_primes).
narrative_ontology:constraint_victim(mil_std_461g_emi_control, small_defense_contractors).
narrative_ontology:constraint_victim(mil_std_461g_emi_control, cots_hardware_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a small contractor, the standard is a costly barrier to entry.
% χ = 0.5 * 1.5 (powerless) * 0.8 (local) = 0.6. This is high extraction.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the DoD, it's a pure coordination tool ensuring platform reliability.
% χ = 0.5 * -0.2 (institutional) * 1.0 (national) = -0.1. Negative extraction.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, it's a system with a valid coordination function that also
% creates asymmetric costs and requires active enforcement. This is the
% canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mil_std_461g_emi_control_tests).

test(perspectival_gap_contractor_vs_dod) :-
    constraint_indexing:constraint_classification(mil_std_461g_emi_control, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mil_std_461g_emi_control, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(mil_std_461g_emi_control, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties_present) :-
    % Verify all three conditions for Tangled Rope are met.
    domain_priors:requires_active_enforcement(mil_std_461g_emi_control),
    narrative_ontology:constraint_beneficiary(mil_std_461g_emi_control, _), % Derives has_coordination_function
    narrative_ontology:constraint_victim(mil_std_461g_emi_control, _).     % Derives has_asymmetric_extraction

:- end_tests(mil_std_461g_emi_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The key to this story is the reclassification of the analytical perspective
 * from 'Mountain' to 'Tangled Rope'. While the standard is based on the laws
 * of physics (a Mountain), the standard itself is a constructed artifact. Its
 * high base extractiveness (0.5) makes a 'Mountain' classification impossible
 * (requires E <= 0.15). The standard clearly has a coordination function
 * (beneficiaries exist) and asymmetric extraction (victims exist), and it
 * requires active enforcement. These are the three definitional pillars of a
 * Tangled Rope. This perspectival gap—where the institution sees a Rope and
 * the subject sees a Snare—is resolved by the analytical view of a Tangled Rope.
 *
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification correctly identifies that MIL-STD-461G is not
 * pure extraction (a Snare) nor pure coordination (a Rope). It acknowledges the
 * valid need for EMI control while simultaneously accounting for the significant,
 * asymmetrically distributed costs of compliance, preventing a misclassification
 * that would ignore either its function or its harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_mil_std_461g_1,
    'To what extent does the standard's "tailoring" clause genuinely reduce costs for small contractors versus being used to increase severity for critical systems?',
    'Audit of tailoring decisions across 100 DoD acquisition programs, comparing cost impacts on small vs. large contractors.',
    'If tailoring primarily benefits small contractors, the constraint leans more towards Rope. If it primarily increases severity, it reinforces the Snare perspective.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mil_std_461g_emi_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness > 0.46 requires temporal data.
% This models a slight "gold-plating" effect over the standard's lifecycle,
% where compliance costs and bureaucratic overhead gradually increase.

% Theater ratio over time:
narrative_ontology:measurement(mil_std_461g_tr_t0, mil_std_461g_emi_control, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mil_std_461g_tr_t5, mil_std_461g_emi_control, theater_ratio, 5, 0.08).
narrative_ontology:measurement(mil_std_461g_tr_t10, mil_std_461g_emi_control, theater_ratio, 10, 0.1).

% Extraction over time:
narrative_ontology:measurement(mil_std_461g_ex_t0, mil_std_461g_emi_control, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mil_std_461g_ex_t5, mil_std_461g_emi_control, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mil_std_461g_ex_t10, mil_std_461g_emi_control, base_extractiveness, 10, 0.5).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This is a technical standard defining interoperability, a classic information standard.
narrative_ontology:coordination_type(mil_std_461g_emi_control, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */