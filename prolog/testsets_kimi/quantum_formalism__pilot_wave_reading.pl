% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__pilot_wave_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quantum_formalism__pilot_wave_reading
 *   human_readable: Pilot-Wave (Bohmian) Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The pilot-wave (Bohmian) reading of quantum mechanics interprets the
 *   universal wavefunction as a physically real field on configuration space
 *   that guides particles along definite trajectories. It eliminates the
 *   measurement problem and observer-dependence but at the cost of accepting
 *   action-at-a-distance via the quantum potential and a privileged role for
 *   position measurements. This constraint story treats the reading as a
 *   coordination mechanism for realist foundational physics: it organizes a
 *   global research program around deterministic trajectories while facing
 *   substantial institutional resistance from the operationalist mainstream.
 *   The authored metrics are descriptively independent of the claimed type.
 *
 * KEY AGENTS:
 *   - bohmian_researchers: agenda_setters who develop and enforce the trajectory framework within their research program (moderate power, constrained exit)
 *   - realist_foundationalists: beneficiaries who draw ontological vindication from the reading (moderate power, mobile exit)
 *   - quantum_foundations_students: junior beneficiaries who gain conceptual clarity but face labor-market constraints (powerless, constrained exit)
 *   - mainstream_physics_community: institutional observers who dominate the field operationally and regard the reading as marginal (institutional, analytical exit)
 *   - empiricist_physicists: excluded voices who regard interpretive debate as metaphysically empty (institutional, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.35).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.25).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot-Wave (Bohmian) Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '31b781a8-55a6-4e1e-9b07-302c038d31f3').
narrative_ontology:cs_kernel_codification('31b781a8-55a6-4e1e-9b07-302c038d31f3', formalized).
narrative_ontology:cs_authority_grounding('31b781a8-55a6-4e1e-9b07-302c038d31f3', expertise).
narrative_ontology:cs_interpretation_layer_present('31b781a8-55a6-4e1e-9b07-302c038d31f3').
narrative_ontology:cs_reading_relation('31b781a8-55a6-4e1e-9b07-302c038d31f3', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('31b781a8-55a6-4e1e-9b07-302c038d31f3', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_axiom('31b781a8-55a6-4e1e-9b07-302c038d31f3', foundational, particle_trajectories_always_exist).
narrative_ontology:cs_axiom_status(particle_trajectories_always_exist, holdable).
narrative_ontology:cs_axiom_grounding('31b781a8-55a6-4e1e-9b07-302c038d31f3', particle_trajectories_always_exist, instrumental).
narrative_ontology:cs_axiom('31b781a8-55a6-4e1e-9b07-302c038d31f3', foundational, wavefunction_physical_field_on_configuration_space).
narrative_ontology:cs_axiom_status(wavefunction_physical_field_on_configuration_space, holdable).
narrative_ontology:cs_axiom_grounding('31b781a8-55a6-4e1e-9b07-302c038d31f3', wavefunction_physical_field_on_configuration_space, instrumental).
narrative_ontology:cs_reference_frame('31b781a8-55a6-4e1e-9b07-302c038d31f3', deterministic_trajectory_realism).
narrative_ontology:cs_drift_state('31b781a8-55a6-4e1e-9b07-302c038d31f3', contemporary_physics_practice, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('31b781a8-55a6-4e1e-9b07-302c038d31f3', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_researchers).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, realist_foundationalists).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, quantum_foundations_students).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, deterministic_evolution).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, particle_realism).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, observer_independence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and extend pilot-wave theory, publish in foundations journals, teach the interpretation to students, and seek funding for research programs built on deterministic trajectories. Their professional identity is tied to the reading's viability; exit requires leaving active physics research or switching to mainstream quantum information, both costly given specialized training.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_researchers, agenda_setter,
    moderate, generational, constrained, global).

% Draw on pilot-wave theory as a rigorous proof-of-concept that quantum mechanics can be understood deterministically with definite particle positions. They cite it in foundational debates and textbooks. Exit is mobile because they can draw on spontaneous collapse, many-worlds, or epistemic interpretations as alternative resources.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, realist_foundationalists, beneficiary,
    moderate, generational, mobile, global).

% Encounter pilot-wave theory in graduate courses or summer schools as a concrete realist alternative to Copenhagen. They benefit from the conceptual clarity of definite trajectories but face structural pressure from the mainstream job market to avoid interpretive specialization. Exit is constrained by advisor networks and hiring committee preferences.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, quantum_foundations_students, beneficiary,
    powerless, biographical, constrained, national).

% Conducts quantum physics research within operational, Copenhagen, or decoherence frameworks, generally treating interpretive questions as secondary or meaningless. They observe the pilot-wave reading from a position of institutional dominance, occasionally engaging with it only to dismiss its nonlocality or metaphysical excess.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, mainstream_physics_community, observer,
    institutional, civilizational, analytical, global).

% Regard interpretive questions as metaphysically empty and the pilot-wave reading as unnecessary scaffolding on the formalism. They would object to resources being spent on trajectory research but are largely excluded from Bohmian workshops and funding panels.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, empiricist_physicists, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a deterministic, observer-independent ontology for quantum mechanics in which all particles have definite positions at all times, eliminating the measurement problem by treating measurement as ordinary interaction and coordinating realist foundational research around trajectory-based models.
% TRANSFER_FUNCTION: Moves explanatory burden from wavefunction collapse and observer-dependent outcomes to a universally valid Schrodinger evolution plus guidance equation; transfers intellectual investment from operationalist stances to ontological claims about particle trajectories and a field on configuration space.
% ABSENT_VOICES: Empiricist and strictly operationalist physicists, who regard the pilot-wave reading as metaphysical excess, are structurally underrepresented in Bohmian research venues and funding bodies; conversely, researchers seeking Lorentz-invariant or field-theoretic alternatives to configuration-space realism have limited voice within the orthodox Bohmian program.
% DISAPPEARANCE_RATIONALE: The Bohmian mechanics research program, including specialized journals, conferences, and trajectory-based computational methods in quantum chemistry, depends on this reading for its organizing ontology. If the reading vanished, these researchers would need to migrate to collapse models, many-worlds, or operationalist frameworks, and the realist-determinist niche in quantum foundations would contract significantly.
% FOUNDING_PROBLEM: The Copenhagen interpretation's reliance on observer-dependent measurement collapse and the apparent impossibility of a deterministic, realist quantum ontology.
% FOUNDING_PROBLEM_CORROBORATION: Advocates of spontaneous collapse (GRW) and many-worlds both corroborate that the measurement problem remains unresolved in operationalist frameworks; empiricist physicists deny the problem is genuine, asserting that decoherence or instrumentalism suffices. The corroboration comes from outside the Bohmian beneficiary set but within the broader foundations community.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__pilot_wave_reading_tests).
:- end_tests(quantum_formalism__pilot_wave_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.35 reflects the genuine ontological costs imposed on adherents (nonlocality, configuration-space field, empty branches) rather than material extraction. Suppression 0.25 is low because the reading does not actively suppress competing interpretations; its persistence does not depend on coercion. Theater 0.30 captures the performative aspect of 'classical restoration' that masks the highly non-classical wavefunction. Accessibility collapse 0.40: for committed adherents alternatives become intellectually unattractive, but trained physicists routinely maintain multiple interpretive frameworks. Resistance 0.75 reflects the mainstream operationalist hegemony and historical marginalization of hidden-variable approaches. Measurements share a single time grid aligned to the interval endpoints.
 *
 * PERSPECTIVAL GAP:
 *   From the Bohmian researcher seat, the constraint is a rope: it solves the measurement problem deterministically and coordinates a viable research program around definite trajectories. From the mainstream operationalist seat, the same structure appears as unnecessary metaphysical ornamentation extracting intellectual effort from tractable computational problems. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Bohmian researchers and realist foundationalists sit on the beneficiary side: the reading subsidizes their research program and ontological commitments. Students benefit from conceptual clarity but with weaker exit options. The mainstream community is not governed by the constraint; it observes from outside, hence no directionality is computed for the observer seat. Empiricist physicists are excluded from the conversation rather than governed.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading was built to solve the measurement problem and restore determinism. That problem remains live in foundations, so mandatrophy is not declared resolved. The constraint persists because its founding function is still unmet by operationalist alternatives, not because it has atrophied into theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wavefunction_ontology_status,
    'Is the wavefunction in pilot-wave theory a physically real field on configuration space, or a mathematical artifact representing incomplete information?',
    'Experimental or theoretical demonstration that the wavefunction can be measured as a field quantity independent of particle positions, or conversely, construction of a deeper theory in which the wavefunction emerges from local beables.',
    'If the wavefunction is not ontologically real, the reading collapses to a simpler hidden-variable theory or collapses entirely; if it is real, the reading maintains its current structure but carries the full metaphysical burden of configuration-space fields.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wavefunction_ontology_status, conceptual, 'Whether the wavefunctions ontological status as a field is structurally essential or dispensable.').

omega_variable(
    classical_restoration_veracity,
    'Does the pilot-wave reading genuinely restore classical ontology, or does the configuration-space wavefunction introduce a deeper non-classical element than the indeterminism it replaces?',
    'Assessment of whether any subsequent theory reduces the configuration-space field to local spacetime structures; sociological tracking of whether physicists treat the theory as genuinely classical or as a different form of quantum strangeness.',
    'If the classical ontology claim is theater, the readings theater_ratio is higher than authored and its extraction (cognitive cost of accepting nonlocal configuration-space fields) is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classical_restoration_veracity, conceptual, 'Whether classical restoration is substantive or performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__pilot_wave_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(quan_tr_t14, quantum_formalism__pilot_wave_reading, theater_ratio, 14, 0.15).
narrative_ontology:measurement(quan_tr_t28, quantum_formalism__pilot_wave_reading, theater_ratio, 28, 0.2).
narrative_ontology:measurement(quan_tr_t42, quantum_formalism__pilot_wave_reading, theater_ratio, 42, 0.3).
narrative_ontology:measurement(quan_tr_t56, quantum_formalism__pilot_wave_reading, theater_ratio, 56, 0.28).
narrative_ontology:measurement(quan_tr_t70, quantum_formalism__pilot_wave_reading, theater_ratio, 70, 0.3).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__pilot_wave_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(quan_be_t14, quantum_formalism__pilot_wave_reading, base_extractiveness, 14, 0.3).
narrative_ontology:measurement(quan_be_t28, quantum_formalism__pilot_wave_reading, base_extractiveness, 28, 0.35).
narrative_ontology:measurement(quan_be_t42, quantum_formalism__pilot_wave_reading, base_extractiveness, 42, 0.4).
narrative_ontology:measurement(quan_be_t56, quantum_formalism__pilot_wave_reading, base_extractiveness, 56, 0.38).
narrative_ontology:measurement(quan_be_t70, quantum_formalism__pilot_wave_reading, base_extractiveness, 70, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quantum_formalism__pilot_wave_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, many_worlds_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the quantum_formalism kernel, decomposed from the colloquial label 'quantum mechanics' into structurally distinct interpretive commitments. The pilot-wave reading instantiates deterministic trajectory realism; sibling readings instantiate observer-dependent collapse and branching-universal realism respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
