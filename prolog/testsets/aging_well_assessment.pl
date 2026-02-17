% ============================================================================
% CONSTRAINT STORY: aging_longevity_tests
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_aging_longevity_tests, []).

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
 * * constraint_id: aging_longevity_tests
 * human_readable: The Commercialized Functional Aging Assessment System
 * domain: health/economic
 * * SUMMARY:
 * A suite of physical proxy tests (e.g., Sitting-Rising, Walking Speed, Grip Strength)
 * originally used for research, now commercialized into a system of assessment,
 * intervention, and billing. While claiming to coordinate healthy aging, the system
 * extracts significant value from patients and creates dependency. The underlying
 * biological realities are Mountain-like, but the system built upon them is a
 * constructed constraint.
 * * KEY AGENTS:
 * - The Aging Patient: Subject (Powerless), faces high costs and pressure.
 * - Healthcare Providers/Industry: Beneficiary (Institutional), profits from the system.
 * - Public Health Analyst: Auditor (Analytical), observes both coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.52), representing the financial costs, psychological
% burden, and over-medicalization imposed by the commercial system.
domain_priors:base_extractiveness(aging_longevity_tests, 0.52).
domain_priors:suppression_score(aging_longevity_tests, 0.45).
domain_priors:theater_ratio(aging_longevity_tests, 0.35).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(aging_longevity_tests, extractiveness, 0.52).
narrative_ontology:constraint_metric(aging_longevity_tests, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(aging_longevity_tests, theater_ratio, 0.35).

% Constraint self-claim (what does the constraint claim to be?)
% The system claims to be a pure coordination mechanism for health.
narrative_ontology:constraint_claim(aging_longevity_tests, tangled_rope).
narrative_ontology:human_readable(aging_longevity_tests, "The Commercialized Functional Aging Assessment System").

% Binary flags
% The system requires active promotion, certification, and integration into
% billing systems to maintain its dominance over alternative approaches.
domain_priors:requires_active_enforcement(aging_longevity_tests).

% Structural property derivation hooks for Tangled Rope classification.
% has_coordination_function/1 is derived from constraint_beneficiary/2.
% has_asymmetric_extraction/1 is derived from constraint_victim/2.
narrative_ontology:constraint_beneficiary(aging_longevity_tests, healthcare_providers).
narrative_ontology:constraint_victim(aging_longevity_tests, aging_patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE AGING PATIENT (SNARE)
% The system is experienced as a costly, coercive trap. Poor scores lead to
% expensive, mandatory interventions, creating a cycle of dependency.
% χ = 0.52 * 1.5 (powerless) * 1.0 (national) = 0.78.
constraint_indexing:constraint_classification(aging_longevity_tests, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE HEALTHCARE PROVIDER (ROPE)
% From the institutional view, the system is a valuable coordination tool. It
% standardizes care, provides clear metrics, and creates a reliable revenue stream.
% χ = 0.52 * -0.2 (institutional) * 1.0 (national) = -0.104.
constraint_indexing:constraint_classification(aging_longevity_tests, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the genuine coordination function for providers and the
% asymmetric extraction imposed on patients. The system is a hybrid.
% χ = 0.52 * 1.15 (analytical) * 1.2 (global) = 0.7176.
constraint_indexing:constraint_classification(aging_longevity_tests, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aging_longevity_tests_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between the patient (snare) and provider (rope).
    constraint_indexing:constraint_classification(aging_longevity_tests, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aging_longevity_tests, rope, context(agent_power(institutional), _, _, _)).

test(tangled_rope_structure_validation) :-
    % A Tangled Rope requires high extraction, enforcement, a beneficiary, and a victim.
    domain_priors:base_extractiveness(aging_longevity_tests, E), E >= 0.46,
    domain_priors:requires_active_enforcement(aging_longevity_tests),
    narrative_ontology:constraint_beneficiary(aging_longevity_tests, _),
    narrative_ontology:constraint_victim(aging_longevity_tests, _).

:- end_tests(aging_longevity_tests_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This regeneration corrects the original file's conceptual error. The original
 * conflated the biological 'Mountain' of aging with the 'constructed system'
 * built to measure it. By re-framing the constraint as the commercialized system,
 * the high extraction (0.52) and suppression (0.45) become justifiable. This
 * creates a classic Tangled Rope scenario.
 * The Perspectival Gap is sharp: for the patient, it's a Snare of high costs and
 * medical dependency. For the provider, it's a Rope that coordinates patient
 * management and billing. The analytical view recognizes both functions co-exist,
 * hence 'Tangled Rope'. The addition of beneficiary and victim facts resolves
 * the original lint errors.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. It prevents the system from
 * accepting the institutional claim that this is a pure 'Rope' for health
 * coordination. By acknowledging the asymmetric extraction felt by the 'victim'
 * group (patients), the model correctly identifies the coercive, extractive
 * dimension that coexists with the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_aging_system_net_effect,
    'Does the commercialized assessment system produce a net positive health outcome compared to non-medicalized, holistic approaches?',
    'Longitudinal cohort study comparing outcomes and costs between populations using the system vs. those using alternative wellness strategies.',
    'If net positive, the system is a high-overhead Rope. If net negative or neutral, it is a pure Snare disguised as coordination.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(aging_longevity_tests, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's evolution from a research tool into a
% commercial system. Extraction accumulated as the system was monetized.
% Required because base_extractiveness (0.52) > 0.46.

% Theater ratio over time:
narrative_ontology:measurement(aging_longevity_tests_tr_t0, aging_longevity_tests, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aging_longevity_tests_tr_t5, aging_longevity_tests, theater_ratio, 5, 0.20).
narrative_ontology:measurement(aging_longevity_tests_tr_t10, aging_longevity_tests, theater_ratio, 10, 0.35).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(aging_longevity_tests_ex_t0, aging_longevity_tests, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aging_longevity_tests_ex_t5, aging_longevity_tests, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(aging_longevity_tests_ex_t10, aging_longevity_tests, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system functions as a standard for measuring and classifying patient status.
narrative_ontology:coordination_type(aging_longevity_tests, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */