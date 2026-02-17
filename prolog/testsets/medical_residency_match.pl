% ============================================================================
% CONSTRAINT STORY: medical_residency_match
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-23
% ============================================================================

:- module(constraint_medical_residency_match, []).

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
 * * constraint_id: medical_residency_match
 * human_readable: The NRMP Medical Residency Match
 * domain: economic/social
 * * SUMMARY:
 * The "Match" is a centralized clearinghouse using a stable matching algorithm
 * (Roth-Peranson) to pair medical students with residency programs. It was
 * created to solve market unraveling ("exploding offers") but now functions
 * as a mandatory, legally binding bottleneck that suppresses wage competition
 * and removes individual bargaining power for new doctors.
 * * KEY AGENTS:
 * - Medical Graduates: Subjects who must participate to become licensed physicians.
 * - Hospital Networks: Beneficiaries who receive a predictable labor supply at suppressed wages.
 * - NRMP/AAMC: Institutional enforcers of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(medical_residency_match, 0.60). % High extraction via wage suppression.
domain_priors:suppression_score(medical_residency_match, 0.95).   % Participation is mandatory; side-deals are prohibited and professionally sanctioned.
domain_priors:theater_ratio(medical_residency_match, 0.10).       % The system is highly functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(medical_residency_match, extractiveness, 0.60).
narrative_ontology:constraint_metric(medical_residency_match, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(medical_residency_match, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a fair coordination mechanism to solve a market failure.
narrative_ontology:constraint_claim(medical_residency_match, tangled_rope).
narrative_ontology:human_readable(medical_residency_match, "The NRMP Medical Residency Match").

% Binary flags
domain_priors:requires_active_enforcement(medical_residency_match). % Required for Tangled Rope.

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(medical_residency_match, hospital_networks).
narrative_ontology:constraint_victim(medical_residency_match, medical_graduates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the student, the Match is a trap. Exit is impossible without abandoning
% one's career. The outcome is legally binding with no room for negotiation.
% χ = 0.60 * 1.5 (powerless) * 1.0 (national) = 0.90. This is a clear Snare.
constraint_indexing:constraint_classification(medical_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the hospital system, it's a pure coordination mechanism that solves a
% chaotic hiring market, ensuring a stable, predictable influx of labor.
% χ = 0.60 * -0.2 (institutional) * 1.0 (national) = -0.12. This is a clear Rope.
constraint_indexing:constraint_classification(medical_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both sides: a genuine coordination function (solving market
% unraveling) tangled with severe, asymmetric extraction (wage suppression).
% It requires active enforcement, has beneficiaries and victims. This is the
% canonical definition of a Tangled Rope.
% χ = 0.60 * 1.15 (analytical) * 1.2 (global) = 0.828.
constraint_indexing:constraint_classification(medical_residency_match, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_residency_match_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between the student (powerless) and hospital (institutional).
    constraint_indexing:constraint_classification(medical_residency_match, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(medical_residency_match, rope, context(agent_power(institutional), _, mobile, _)).

test(analytical_observer_detects_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(medical_residency_match, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction_and_suppression) :-
    % Verify the base metrics meet the criteria for a high-coercion constraint.
    domain_priors:base_extractiveness(medical_residency_match, E),
    domain_priors:suppression_score(medical_residency_match, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(medical_residency_match_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Medical Residency Match is a canonical example of a Tangled Rope. It
 * originated to solve a real coordination problem (a Rope function) but has
 * since been captured by institutional interests to create a system of
 * asymmetric extraction (a Snare function).
 *
 * * Perspectival Gap:
 * The gap is stark. For students ('powerless', 'trapped'), the system is a
 * coercive Snare with no alternatives. For hospital networks ('institutional',
 * 'mobile'), it is a beneficial Rope that stabilizes their labor market and
 * suppresses wages. The system's stability depends on these opposing views
 * never being reconciled.
 *
 * * MANDATROPHY ANALYSIS:
 * The 'tangled_rope' classification is critical for preventing Mandatrophy.
 * A naive analysis might see only the coordination function and call it a
 * 'Rope', ignoring the immense extraction. Another might see only the
 * coercion and call it a 'Snare', ignoring the genuine market failure it
 * solves. Tangled Rope correctly identifies that it is both: a coordination
 * mechanism that has been weaponized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_medical_residency_match,
    'Is the Match system's antitrust exemption a permanent feature of US labor law, or a temporary artifact vulnerable to legal challenge?',
    'A Supreme Court ruling overturning the precedent set in Jung v. AAMC, or new federal legislation.',
    'If overturned, the Tangled Rope unravels. The system would either collapse into a chaotic decentralized market or be replaced by a less extractive coordination mechanism (a true Rope).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(medical_residency_match, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint shows a gradual increase
% in extraction as hospital consolidation increased and resident unions lost
% power, alongside a minor increase in performative bureaucracy (theater).
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(mrm_tr_t0, medical_residency_match, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mrm_tr_t5, medical_residency_match, theater_ratio, 5, 0.08).
narrative_ontology:measurement(mrm_tr_t10, medical_residency_match, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(mrm_ex_t0, medical_residency_match, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mrm_ex_t5, medical_residency_match, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(mrm_ex_t10, medical_residency_match, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The Match is a textbook example of a resource allocation mechanism.
narrative_ontology:coordination_type(medical_residency_match, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */