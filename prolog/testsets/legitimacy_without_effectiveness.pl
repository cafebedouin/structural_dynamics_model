% ============================================================================
% CONSTRAINT STORY: legitimacy_without_effectiveness
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_legitimacy_without_effectiveness, []).

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
 * * constraint_id: legitimacy_without_effectiveness
 * human_readable: The Hollow Mandate
 * domain: political
 * * SUMMARY:
 * A condition where a governing body retains its legal and social recognition
 * as the rightful authority, but has lost the capacity to fulfill its primary
 * functions (security, infrastructure, economic stability). The state exists
 * primarily to sustain itself through performative acts of governance.
 * * KEY AGENTS:
 * - The Citizen: Subject (Powerless) - Obligated to obey a ghost system.
 * - The Bureaucrat: Beneficiary (Institutional) - Maintaining the facade of office.
 * - The Political Scientist: Auditor (Analytical) - Measuring the "Piton" status.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(legitimacy_without_effectiveness, 0.75). % High: Extraction to maintain the facade.
domain_priors:suppression_score(legitimacy_without_effectiveness, 0.82).   % High: State monopoly prevents alternatives.
domain_priors:theater_ratio(legitimacy_without_effectiveness, 0.91).       % Very High: Purely theatrical governance.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, extractiveness, 0.75).
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, theater_ratio, 0.91).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(legitimacy_without_effectiveness, piton).
narrative_ontology:human_readable(legitimacy_without_effectiveness, "The Hollow Mandate").

% Binary flags
domain_priors:requires_active_enforcement(legitimacy_without_effectiveness).

% Structural property derivation hooks (required for high-extraction constraints)
narrative_ontology:constraint_beneficiary(legitimacy_without_effectiveness, bureaucrat_class).
narrative_ontology:constraint_victim(legitimacy_without_effectiveness, citizen).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the individual, the law is no longer a guide but a trap that extracts
% taxes/compliance without providing safety.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% To the institution, the mandate is a Rope—the essential coordination tool
% that prevents total social dissolution, justifying its own existence.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Detection of "Piton" status: high theater_ratio indicates a constraint
% maintained by inertia rather than utility.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(legitimacy_without_effectiveness, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_without_effectiveness_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(legitimacy_without_effectiveness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_without_effectiveness, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless = snare,
    TypeInstitutional = rope.

test(piton_signature) :-
    % Verify the analytical perspective correctly identifies the Piton status.
    constraint_indexing:constraint_classification(legitimacy_without_effectiveness, piton, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % High extraction (> 0.46) is a key feature.
    narrative_ontology:constraint_metric(legitimacy_without_effectiveness, extractiveness, E),
    E >= 0.46.

:- end_tests(legitimacy_without_effectiveness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) is high because the system consumes resources
 * purely to perpetuate its own existence (salaries, symbolic rituals) rather
 * than providing social output. The suppression score (0.82) is high because
 * the state, however ineffective, maintains a monopoly on legitimate force,
 * preventing alternatives from arising.
 *
 * [RESOLVED MANDATROPHY]
 * This constraint is not a "Mountain" of natural law; it is a "Piton."
 * The Mandatrophy is resolved by identifying that the high "Theater Ratio"
 * (0.91) differentiates it from a functioning Rope or Snare. The system "performs"
 * legitimacy through ceremonies and legalisms to mask its operational
 * non-existence, which is the defining characteristic of a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_collapse_velocity,
    'Will the Piton fail suddenly (Snap) or erode into a decentralized Scaffold?',
    'Measure of "Alternative Sovereignty" adoption vs. legacy compliance rates.',
    'If snap: Violent transition to Snare; If erosion: Potential for new Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(legitimacy_without_effectiveness, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint models a state that decayed from a functional, if extractive,
% system into a purely theatrical one. The data shows rising theater and
% extraction as function is replaced by performance.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(lwe_tr_t0, legitimacy_without_effectiveness, theater_ratio, 0, 0.30).
narrative_ontology:measurement(lwe_tr_t5, legitimacy_without_effectiveness, theater_ratio, 5, 0.70).
narrative_ontology:measurement(lwe_tr_t10, legitimacy_without_effectiveness, theater_ratio, 10, 0.91).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(lwe_ex_t0, legitimacy_without_effectiveness, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(lwe_ex_t5, legitimacy_without_effectiveness, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(lwe_ex_t10, legitimacy_without_effectiveness, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The state's claim to legitimacy is based on its role as the ultimate
% enforcement mechanism for social order.
narrative_ontology:coordination_type(legitimacy_without_effectiveness, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */