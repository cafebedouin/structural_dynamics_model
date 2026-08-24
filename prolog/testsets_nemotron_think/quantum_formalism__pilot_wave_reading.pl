% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Pilot Wave Interpretation of Quantum Mechanics (de Broglie-Bohm)
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The pilot wave reading (de Broglie-Bohm theory) interprets the quantum
 *   formalism as describing particles with definite positions at all times,
 *   guided by a physically real wavefunction (the 'pilot wave') evolving in
 *   configuration space. Measurement reveals pre-existing positions; the
 *   observer plays no fundamental role; determinism is restored at the cost
 *   of explicit nonlocality. This reading has persisted since 1952 as a
 *   minority research program, structurally marginalized by the dominant
 *   Copenhagen and Many-Worlds readings which control curricula, journals,
 *   hiring, and funding. The constraint is the interpretive commitment itself
 *   — adopting it imposes career costs (extraction) while providing
 *   theoretical coherence (coordination). The institutional arrangement that
 *   marginalizes it requires active enforcement (gatekeeping).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.45).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.6).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot Wave Interpretation of Quantum Mechanics (de Broglie-Bohm)").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '4ec176f6-d05b-499e-9048-d9abaa3bfaff').
narrative_ontology:cs_kernel_codification('4ec176f6-d05b-499e-9048-d9abaa3bfaff', formalized).
narrative_ontology:cs_authority_grounding('4ec176f6-d05b-499e-9048-d9abaa3bfaff', practice).
narrative_ontology:cs_interpretation_layer_present('4ec176f6-d05b-499e-9048-d9abaa3bfaff').
narrative_ontology:cs_reading_relation('4ec176f6-d05b-499e-9048-d9abaa3bfaff', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('4ec176f6-d05b-499e-9048-d9abaa3bfaff', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_axiom('4ec176f6-d05b-499e-9048-d9abaa3bfaff', foundational, particle_positions_ontologically_definite).
narrative_ontology:cs_axiom_status(particle_positions_ontologically_definite, holdable).
narrative_ontology:cs_axiom_grounding('4ec176f6-d05b-499e-9048-d9abaa3bfaff', particle_positions_ontologically_definite, deontological).
narrative_ontology:cs_axiom('4ec176f6-d05b-499e-9048-d9abaa3bfaff', foundational, wavefunction_ontologically_real_physical_field).
narrative_ontology:cs_axiom_status(wavefunction_ontologically_real_physical_field, holdable).
narrative_ontology:cs_axiom_grounding('4ec176f6-d05b-499e-9048-d9abaa3bfaff', wavefunction_ontologically_real_physical_field, deontological).
narrative_ontology:cs_axiom('4ec176f6-d05b-499e-9048-d9abaa3bfaff', foundational, classical_ontology_restored_via_hidden_variables).
narrative_ontology:cs_axiom_status(classical_ontology_restored_via_hidden_variables, holdable).
narrative_ontology:cs_axiom_grounding('4ec176f6-d05b-499e-9048-d9abaa3bfaff', classical_ontology_restored_via_hidden_variables, instrumental).
narrative_ontology:cs_reference_frame('4ec176f6-d05b-499e-9048-d9abaa3bfaff', deterministic_particle_ontology).
narrative_ontology:cs_drift_state('4ec176f6-d05b-499e-9048-d9abaa3bfaff', post_bell_theorem_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4ec176f6-d05b-499e-9048-d9abaa3bfaff', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, standard_qm_establishment).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, many_worlds_proponents).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, bohmian_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_practitioners).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, deterministic_hidden_variables_restore_classical_ontology).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, wavefunction_as_physical_field).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, measurement_reveals_pre_existing_positions).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, observer_eliminable_from_fundamental_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physicists and philosophers who develop and defend the de Broglie-Bohm theory. They bear career costs: fewer faculty positions, marginalization in major journals, exclusion from standard curricula, difficulty placing students. They gain theoretical coherence: a deterministic, observer-independent ontology that solves the measurement problem without collapse or branching. Exit is constrained — switching interpretations mid-career means abandoning their research program and professional identity.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_practitioners, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, bohmian_practitioners, beneficiary).

% The mainstream physics community controlling curricula, major journals (PRL, Nature Physics, etc.), hiring committees, funding agencies, and conference circuits. They set the agenda by defining what counts as 'mainstream' quantum foundations. They benefit from the pilot wave's marginalization — it removes a coherent deterministic competitor, simplifies teaching, and protects the institutional investment in the Copenhagen/Many-Worlds framework. Their exit options are arbitrage-grade: they can adopt Bohmian mechanics if it ever becomes advantageous, but face no pressure to do so.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, standard_qm_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Physicists and philosophers advocating the Everett/Many-Worlds interpretation. They benefit from the pilot wave's marginalization because they occupy the 'deterministic, no-collapse' niche without the nonlocality stigma. They have mobile exit options — they can shift between Many Worlds, decoherence-based approaches, or quantum Bayesianism as institutional winds change. Their power derives from alignment with the dominant cosmological and quantum-information paradigms.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, many_worlds_proponents, beneficiary,
    powerful, biographical, mobile, global).

% Philosophers who analyze the interpretive landscape without professional stakes in any single reading. They have analytical exit options — their role is to map the structural relations between readings, not to inhabit one. They provide the corroboration for the founding problem's status (live/contested) from outside the benefiting parties.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, philosophy_of_physics_community, observer,
    organized, generational, analytical, global).

% Graduate and undergraduate physics students who receive standard quantum mechanics education. They are structurally excluded from the pilot wave reading — it is absent from almost all standard curricula, textbooks, and qualifying exams. They would object to the narrowness of their training if they knew a coherent deterministic alternative existed, but they are trapped in the institutional pipeline that presents the Copenhagen/Many-Worlds dichotomy as exhaustive.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, physics_students, excluded,
    powerless, immediate, trapped, global).

% Experimental physicists working on Bell tests, matter-wave interferometry, protective measurements, and other foundations-relevant experiments. They are observers because their work constrains all readings equally (empirical adequacy is shared). They have mobile exit options — they can collaborate with any interpretive community. Their situation is neutral: they need the formalism to work, not any particular ontology.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, experimentalists_testing_foundations, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a deterministic, observer-independent ontology for quantum mechanics that solves the measurement problem by postulating definite particle positions guided by a physical wavefield in configuration space.
% TRANSFER_FUNCTION: Moves career capital and institutional recognition from pilot wave proponents to the dominant interpretations; moves theoretical coherence (determinism, no collapse, no branching, observer elimination) to the proponents.
% ABSENT_VOICES: Physics students and early-career researchers who are not exposed to the pilot wave interpretation in standard curricula; they would object to the narrowness of their training if they knew the alternative. Also: physicists in adjacent fields (quantum gravity, quantum information) who might find Bohmian tools useful but never encounter them.
% DISAPPEARANCE_RATIONALE: If the pilot wave reading vanished overnight, the Bohmian mechanics research program (literature, practitioners, students, conferences, journals like Foundations of Physics) would collapse. The mainstream would lose a coherent deterministic alternative, but standard quantum mechanics would continue largely unchanged. The conceptual landscape of quantum foundations would lose its only particle-ontology interpretation.
% FOUNDING_PROBLEM: The measurement problem in quantum mechanics — the lack of a clear account of when and how superpositions become definite outcomes, and the role of the observer in the transition from possible to actual.
% FOUNDING_PROBLEM_CORROBORATION: Bell (1964) proved hidden variables theories are viable if nonlocal, refuting von Neumann's no-go theorem. Contemporary philosophers of physics (Maudlin, Goldstein, Zanghì, Norsen, Esfeld) attest the measurement problem remains unsolved in the dominant interpretations. The mainstream physics community largely considers it solved or dissolved via decoherence, which is contested — the Born rule derivation in Many Worlds and the preferred-basis problem remain open.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.45) reflects the career capital and institutional access forfeited by bohmian_practitioners — they publish in fewer venues, face hiring barriers, and their students inherit the marginalization. Suppression (0.6) is high because the reading's persistence depends on the dominant establishment actively excluding it from standard curricula and major journals — not merely neglect but structural gatekeeping. Theater ratio (0.2) is low: the Bohmian research program produces genuine technical work (relativistic extensions, quantum equilibrium, numerical methods), not performative compliance. Accessibility collapse (0.4) is moderate: alternatives (Copenhagen, Many Worlds) remain live and institutionally dominant. Resistance (0.7) is high: the reading faces active opposition from the establishment, not passive indifference.
 *
 * PERSPECTIVAL GAP:
 *   From the bohmian_practitioner seat, the constraint is a ROPE — genuine coordination (determinism, no measurement problem, clear ontology) worth the career cost. From the standard_qm_establishment seat, it is a SNARE — a marginalized position they actively suppress to maintain institutional control. From the many_worlds_proponent seat, it is a TANGLED ROPE — a competitor for the 'deterministic interpretation' niche that they benefit from seeing marginalized. The engine computes this divergence; the claimed_type (tangled_rope) reflects the structural reality that BOTH coordination and extraction are present.
 *
 * DIRECTIONALITY LOGIC:
 *   Bohmian practitioners are payers (d ≈ 0.75) — they bear the extraction via career costs, with constrained exit (switching interpretations mid-career is costly). The standard_qm_establishment are agenda_setters (d ≈ 0.1) — they control the institutional machinery that enforces marginalization and benefit from the absence of a viable deterministic competitor. Many_worlds_proponents are beneficiaries (d ≈ 0.2) — they occupy the dominant 'no-collapse' niche without the nonlocality baggage. Philosophy_of_physics and physics_students are observers/excluded — the former analyzes from outside, the latter are denied exposure. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the measurement problem) remains LIVE — decoherence does not solve the single-outcome problem, and the Born rule derivation in Many Worlds remains contested. The pilot wave reading continues to solve a live problem, so mandatrophy is NOT resolved. However, the reading's institutional marginalization has atrophied its coordination reach — it coordinates a small research program but not the broader field. This is not a piton (the function hasn't atrophied, the reach has). The coordination function is real but institutionally contained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_status_of_wavefunction,
    'Is the wavefunction a real physical field in configuration space, or a mathematical tool for computing probabilities?',
    'Conceptual analysis of whether configuration-space fields can be ontologically parsimonious; experimental tests of wavefunction realism (e.g., protective measurement proposals).',
    'If the wavefunction is not a physical field, the pilot wave reading''s central ontological commitment collapses; if it is, the reading gains ontological parity with the quantum state in other interpretations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_status_of_wavefunction, conceptual, 'Whether the pilot wave''s central entity — the wavefunction as physical field — is a coherent ontological category.').

omega_variable(
    nonlocality_mechanism,
    'What is the physical mechanism of the nonlocal guidance? The pilot wave acts instantaneously across space — is this a causal influence, a holistic constraint, or a feature of configuration space geometry?',
    'Theoretical work on relativistic extensions; analysis of whether the nonlocality violates relativistic causality or merely exploits its loopholes (no-signalling).',
    'If the nonlocality is merely mathematical (configuration space), the reading''s claim to restore ''classical ontology'' is undermined; if it requires physical superluminal causation, the reading inherits the very nonlocality it was meant to domesticate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonlocality_mechanism, empirical, 'The physical character of the nonlocal guidance — the reading''s most empirically consequential and ontologically costly commitment.').

omega_variable(
    relativistic_extension_viability,
    'Can the pilot wave reading be extended to a fully relativistic quantum field theory without introducing a preferred foliation or sacrificing empirical adequacy?',
    'Ongoing research on Bohmian QFT (Dürr et al., Struyve, Colin); experimental probes of Lorentz invariance at quantum-gravity scales.',
    'If no relativistic extension exists, the reading is a non-relativistic approximation at best — a scaffold, not a fundamental theory. If a viable extension exists, the reading''s claim to be a fundamental ontology is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relativistic_extension_viability, empirical, 'Whether the reading can survive the transition from quantum mechanics to quantum field theory.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the quantum_formalism kernel, distinct from copenhagen_reading and many_worlds_reading, with its own stable ε and beneficiary/victim structure?',
    'Structural comparison of the three readings'' beneficiary/victim sets, extraction profiles, and coordination functions — the pilot wave reading extracts career capital from its proponents while the dominant readings benefit from institutional capture.',
    'Confirms the ε-invariance principle: each reading instantiates a different constraint with different structural properties, not different measurements of the same constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this constraint is the pilot_wave_reading of kernel quantum_formalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 1952, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1952, quantum_formalism__pilot_wave_reading, theater_ratio, 1952, 0.1).
narrative_ontology:measurement(quan_tr_t1965, quantum_formalism__pilot_wave_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(quan_tr_t1980, quantum_formalism__pilot_wave_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(quan_tr_t1995, quantum_formalism__pilot_wave_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__pilot_wave_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(quan_tr_t2024, quantum_formalism__pilot_wave_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(quan_be_t1952, quantum_formalism__pilot_wave_reading, base_extractiveness, 1952, 0.3).
narrative_ontology:measurement(quan_be_t1965, quantum_formalism__pilot_wave_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(quan_be_t1980, quantum_formalism__pilot_wave_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(quan_be_t1995, quantum_formalism__pilot_wave_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__pilot_wave_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(quan_be_t2024, quantum_formalism__pilot_wave_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1952, quantum_formalism__pilot_wave_reading, suppression_requirement, 1952, 0.7).
narrative_ontology:measurement(quan_su_t1965, quantum_formalism__pilot_wave_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(quan_su_t1980, quantum_formalism__pilot_wave_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(quan_su_t1995, quantum_formalism__pilot_wave_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(quan_su_t2010, quantum_formalism__pilot_wave_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(quan_su_t2024, quantum_formalism__pilot_wave_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.08).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).

% DUAL FORMULATION NOTE:
% The quantum_formalism kernel decomposes into three constraint stories — one per reading — because each reading has a different ε, different beneficiary/victim structure, and different coordination/extraction profile. The pilot wave reading extracts career capital from its proponents; Copenhagen and Many Worlds benefit from institutional dominance. They are not the same constraint measured differently — they are different institutional arrangements grounded in the same formal kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, moderate, 0.75).
constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, institutional, 0.1).
constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, powerful, 0.2).
constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, organized, 0.4).
constraint_indexing:directionality_override(quantum_formalism__pilot_wave_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
