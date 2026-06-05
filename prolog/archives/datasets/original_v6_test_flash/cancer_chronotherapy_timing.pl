% ============================================================================
% CONSTRAINT STORY: cancer_chronotherapy_timing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cancer_chronotherapy_timing, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cancer_chronotherapy_timing
 *   human_readable: The Circadian Lifeline
 *   domain: health/technological
 *
 * SUMMARY:
 *   The timing of immunotherapy, specifically before 3pm, leverages T-cell
 *   congregation around tumors, which can improve cancer treatment efficacy.
 *   This constraint affects patients differently depending on their treatment
 *   schedule. Patients treated within the optimal circadian window experience
 *   improved outcomes, while those treated outside this window receive less
 *   benefit. The medical community is both coordinating to leverage this
 *   knowledge, but there is still extraction between patients based on
 *   scheduling logistics.
 *
 * KEY AGENTS:
 *   - Cancer Patients (Early Treatment): Benefit from increased treatment efficacy (moderate/mobile)
 *   - Cancer Patients (Late Treatment): Experience reduced treatment efficacy (powerless/trapped)
 *   - Oncologists (Standard Practice): Constrained by scheduling logistics but can benefit from improved patient outcomes (institutional/constrained)
 *   - Analytical Observer: Views the constraint from a global perspective (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cancer_chronotherapy_timing, 0.55).
domain_priors:suppression_score(cancer_chronotherapy_timing, 0.4).
domain_priors:theater_ratio(cancer_chronotherapy_timing, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, extractiveness, 0.55).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cancer_chronotherapy_timing, tangled_rope).
narrative_ontology:human_readable(cancer_chronotherapy_timing, "The Circadian Lifeline").
narrative_ontology:topic_domain(cancer_chronotherapy_timing, "health/technological").

domain_priors:requires_active_enforcement(cancer_chronotherapy_timing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, cancer_patients_early_treatment).
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, oncologists_early_treatment).
narrative_ontology:constraint_victim(cancer_chronotherapy_timing, cancer_patients_late_treatment).
narrative_ontology:constraint_victim(cancer_chronotherapy_timing, oncologists_late_treatment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients receiving immunotherapy outside the optimal circadian window experience reduced treatment efficacy, acting as a snare due to lower T-cell congregation at the tumor site.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Patients receiving immunotherapy within the optimal circadian window benefit from increased treatment efficacy due to higher T-cell presence at the tumor site, acting as a rope.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Oncologists are constrained by standard practice and scheduling logistics but also benefit from improved patient outcomes when adhering to circadian timing. They coordinate care, but there is extraction between patients based on when they receive the treatment.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer sees that the practice is a tangled rope, because of the extraction based on timing, and coordination in using circadian timing for improved outcomes, but is not always followed because of scheduling.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cancer_chronotherapy_timing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cancer_chronotherapy_timing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cancer_chronotherapy_timing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cancer_chronotherapy_timing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cancer_chronotherapy_timing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate extraction occurs because patients receiving treatment outside the optimal window experience reduced efficacy. Suppression (0.40): There is moderate suppression due to scheduling constraints, lack of awareness among all practitioners, and patient convenience.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap exists because patients treated at different times of the day experience different outcomes. Patients treated within the optimal circadian window experience improved treatment efficacy. Patients treated outside experience reduced efficacy, and the analytical perspective observes both, as well as the oncologist coordinating this.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients receiving immunotherapy outside the optimal circadian window bear the cost (high d value). Patients receiving the treatment in the optimal window benefit (low d value). Oncologists are constrained by logistics, but benefit from good outcomes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patient_adherence,
    'To what extent can patients adhere to specific treatment times given their daily routines and schedules?',
    'Conduct patient surveys and track appointment adherence rates.',
    'If low adherence, then the benefit of circadian timing is negated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_adherence, empirical, 'Patient adherence to treatment schedules').

omega_variable(
    confounding_factors,
    'Are there other confounding factors that influence treatment efficacy beyond circadian timing?',
    'Conduct multivariate analysis considering patient genetics, lifestyle, and disease stage.',
    'If significant confounding factors exist, then the impact of circadian timing is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confounding_factors, empirical, 'Confounding factors affecting treatment efficacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cancer_chronotherapy_timing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(canc_tr_t0, cancer_chronotherapy_timing, theater_ratio, 0, 0.1).
narrative_ontology:measurement(canc_tr_t5, cancer_chronotherapy_timing, theater_ratio, 5, 0.15).
narrative_ontology:measurement(canc_tr_t10, cancer_chronotherapy_timing, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(canc_be_t0, cancer_chronotherapy_timing, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(canc_be_t5, cancer_chronotherapy_timing, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(canc_be_t10, cancer_chronotherapy_timing, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cancer_chronotherapy_timing, resource_allocation).
narrative_ontology:affects_constraint(cancer_chronotherapy_timing, immunotherapy_effectiveness).
narrative_ontology:affects_constraint(cancer_chronotherapy_timing, cancer_treatment_side_effects).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
