% ============================================================================
% CONSTRAINT STORY: cancer_chronotherapy_timing
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_cancer_chronotherapy_timing, []).

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
 * * constraint_id: cancer_chronotherapy_timing
 * human_readable: The Circadian Lifeline
 * domain: health/technological
 * * SUMMARY:
 * A medical intervention where the timing of immunotherapy (before 3pm)
 * leverages T-cell congregation around tumours. This coordination
 * around circadian rhythms resulted in a nearly doubling of survival time
 * (28 months vs 17 months) for lung cancer patients.
 * * KEY AGENTS:
 * - The Patient: Subject (Powerless) - Dependent on hospital scheduling.
 * - The Hospital Administration: Beneficiary (Institutional) - Managing treatment cycles.
 * - The Chronobiologist: Auditor (Analytical) - Mapping T-cell migration.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is near-zero; this is a timing shift of existing resources.
domain_priors:base_extractiveness(cancer_chronotherapy_timing, 0.05).
% Suppression is moderate; patients are constrained by clinical hours and staff availability.
domain_priors:suppression_score(cancer_chronotherapy_timing, 0.40).
% Theater ratio is low; the evidence is based on a randomised-controlled trial.
domain_priors:theater_ratio(cancer_chronotherapy_timing, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, extractiveness, 0.05).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(cancer_chronotherapy_timing, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(cancer_chronotherapy_timing, rope).
narrative_ontology:human_readable(cancer_chronotherapy_timing, "The Circadian Lifeline").

% Binary flags
narrative_ontology:has_sunset_clause(cancer_chronotherapy_timing). % The logistical constraint is temporary until scheduling adapts.

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, lung_cancer_patients).
narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, oncology_department).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE PATIENT (ROPE)
% For the patient, this is pure coordination that extends life with minimal cost.
% χ = 0.05 * π(powerless:1.5) * σ(regional:0.9) = 0.0675. This is well within Rope thresholds.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE HOSPITAL ADMINISTRATION (SCAFFOLD)
% Busy hospitals view the 3pm window as a temporary logistical support measure
% that must be implemented, creating a scheduling challenge that will be resolved
% over time as protocols adapt.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, scaffold,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))) :-
    narrative_ontology:has_sunset_clause(cancer_chronotherapy_timing).

% PERSPECTIVE 3: THE BIOLOGICAL REALIST (MOUNTAIN)
% From an analytical, universal perspective, the underlying circadian rhythms
% are fixed biological limits ("Mountain") that the therapy must work around.
constraint_indexing:constraint_classification(cancer_chronotherapy_timing, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chronotherapy_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the key agents.
    constraint_indexing:constraint_classification(cancer_chronotherapy_timing, TypePatient, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cancer_chronotherapy_timing, TypeHospital, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cancer_chronotherapy_timing, TypeAnalyst, context(agent_power(analytical), _, _, _)),
    TypePatient == rope,
    TypeHospital == scaffold,
    TypeAnalyst == mountain,
    TypePatient \= TypeHospital,
    TypeHospital \= TypeAnalyst.

test(scaffold_conditions_met) :-
    % Verify that the conditions for a Scaffold classification are met.
    narrative_ontology:has_sunset_clause(cancer_chronotherapy_timing),
    narrative_ontology:constraint_beneficiary(cancer_chronotherapy_timing, _).

:- end_tests(chronotherapy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint generates a rich perspectival gap based on what aspect of the
 * system an agent is focused on.
 * - The Patient (powerless) experiences the outcome: a life-extending coordination
 *   mechanism with no added financial cost. This is a pure Rope.
 * - The Hospital Administration (institutional) experiences the implementation: a
 *   temporary but disruptive logistical challenge that must be supported until
 *   new scheduling systems are permanent. This is a Scaffold.
 * - The Chronobiologist (analytical) sees the underlying mechanism: an immutable
 *   biological law of T-cell migration. This is a Mountain.
 * The low base extractiveness (0.05) and moderate suppression (0.40, from
 * scheduling inflexibility) are key to these classifications. The Scaffold
 * classification is valid because a sunset clause is declared (the logistical
 * support is temporary) and beneficiaries exist (patients and the hospital).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_individual_chronotype,
    'How do individual chronotypes ("morning larks" vs "night owls") shift the optimal 3pm window?',
    'Personalized immune-fluctuation tracking over 4 treatment cycles across a diverse patient cohort.',
    'If variation is significant, the universal "Mountain" of circadian rhythm becomes a personalized "Scaffold" requiring individual calibration.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cancer_chronotherapy_timing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a low-extraction constraint, so temporal data is not strictly
% required, but provided for completeness. The values are stable as the
% constraint is based on a fixed biological process and a one-time protocol shift.

% Theater ratio over time: Remains low as clinical efficacy is the primary driver.
narrative_ontology:measurement(ct_tr_t0, cancer_chronotherapy_timing, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ct_tr_t5, cancer_chronotherapy_timing, theater_ratio, 5, 0.10).
narrative_ontology:measurement(ct_tr_t10, cancer_chronotherapy_timing, theater_ratio, 10, 0.10).

% Extraction over time: Fixed at near-zero; the protocol is a matter of timing, not cost.
narrative_ontology:measurement(ct_ex_t0, cancer_chronotherapy_timing, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(ct_ex_t5, cancer_chronotherapy_timing, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(ct_ex_t10, cancer_chronotherapy_timing, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint is about allocating scarce clinical resources (infusion chairs, staff)
% at the optimal time.
narrative_ontology:coordination_type(cancer_chronotherapy_timing, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */