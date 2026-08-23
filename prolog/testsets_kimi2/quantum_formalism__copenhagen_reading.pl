% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__copenhagen_reading, []).

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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Reading of Quantum Measurement (Collapse as Primitive Ontological Boundary)
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The Copenhagen reading of quantum mechanics treats wavefunction collapse
 *   as a physically irreducible process and measurement as a primitive
 *   ontological boundary. Emerging from Bohr's complementarity framework and
 *   von Neumann's axiomatic codification, it became the dominant interpretive
 *   lens of twentieth-century physics. This constraint story models the
 *   reading as a commitment-system constraint that both coordinates standard
 *   quantum practice and asymmetrically extracts from realist foundational
 *   inquiry by declaring certain questions illegitimate. As one reading of
 *   the contested quantum_formalism kernel, it stands in logical foreclosure
 *   to both the many-worlds reading (which eliminates collapse) and the
 *   pilot-wave reading (which restores deterministic hidden variables).
 *
 * KEY AGENTS:
 *   - mainstream_qm_practitioners: Primary beneficiary (organized/constrained) â gains calculational coherence, pays diffuse epistemic closure cost.
 *   - realist_foundational_researchers: Primary target (moderate/constrained) â bears extraction through pedagogical and funding marginalization.
 *   - physics_pedagogical_authority: Agenda setter (institutional/arbitrage) â administers the boundary between physics and interpretation.
 *   - philosophy_of_physics_observers: Analytical observer â maps structural divergence across kernel readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.55).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.68).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Reading of Quantum Measurement (Collapse as Primitive Ontological Boundary)").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '4b24084f-7ad1-43e0-a03e-d196c59e4fe1').
narrative_ontology:cs_kernel_codification('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', formalized).
narrative_ontology:cs_authority_grounding('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', lineage).
narrative_ontology:cs_interpretation_layer_present('4b24084f-7ad1-43e0-a03e-d196c59e4fe1').
narrative_ontology:cs_reading_relation('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_reading_relation('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', foundational, measurement_as_primitive_ontology).
narrative_ontology:cs_axiom_status(measurement_as_primitive_ontology, holdable).
narrative_ontology:cs_axiom_grounding('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', measurement_as_primitive_ontology, conventional).
narrative_ontology:cs_axiom('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', foundational, irreducible_measurement_indeterminism).
narrative_ontology:cs_axiom_status(irreducible_measurement_indeterminism, holdable).
narrative_ontology:cs_axiom_grounding('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', irreducible_measurement_indeterminism, empirically_contingent).
narrative_ontology:cs_reference_frame('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', complementarity_framework).
narrative_ontology:cs_drift_state('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', post_decoherence_theory, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4b24084f-7ad1-43e0-a03e-d196c59e4fe1', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, mainstream_qm_practitioners).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, realist_foundational_researchers).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, wavefunction_completeness).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_postulate_primitivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Employ the Copenhagen prescriptionâstate-vector reduction and the Born ruleâas the standard calculational framework for quantum predictions. They benefit from a shared language that sidesteps unresolved measurement questions, but their collective practice is constrained by textbook orthodoxy that treats measurement as primitive and deterministic inquiry as outside the scope of physics proper.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, mainstream_qm_practitioners, beneficiary,
    organized, biographical, constrained, global).

% Pursue deterministic or realist completions of quantum theory, including hidden-variables, dynamical-collapse, and many-worlds programs. They bear the cost of marginalization in mainstream curricula, funding panels, and textbook presentations, and must construct parallel institutionsâspecialized journals, conferences, and departmentsâto investigate questions the Copenhagen reading declares meaningless or already settled.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, realist_foundational_researchers, payer,
    moderate, generational, constrained, global).

% Administers the standard quantum-mechanics curriculum, licensing examinations, and textbook canon. Sets the boundary between legitimate calculational technique and 'mere interpretation,' enforcing the primitivity of measurement by excluding decoherence-based or realist derivations from core training and by classifying foundational questions as philosophical rather than physical.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, physics_pedagogical_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Map the structural commitments of the Copenhagen reading and track its divergence from sibling readings of the same kernel. They observe that the reading's classification hinges on whether measurement primitivity is treated as irreducible physical law or as an institutionalized interpretive stipulation.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophy_of_physics_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Furnishes a universal calculational prescription for quantum predictions that does not require solving the measurement problem, enabling experimentalists, theorists, and engineers to coordinate prediction, design, and verification despite radical disagreement about underlying ontology.
% TRANSFER_FUNCTION: Moves epistemic authority and research attention from realist foundational programs to the instrumentalist mainstream by declaring measurement a primitive boundary; transfers the practical license to dismiss the measurement problem from the interpretive superstructure to working physicists.
% ABSENT_VOICES: Bohmian and Everettian researchers are audible in foundations circles but structurally excluded from mainstream pedagogy and major funding streams; more radically, empirical-collapse-modelers and superdeterminist theorists are largely outside the room in which what counts as physics is decided.
% DISAPPEARANCE_RATIONALE: If the Copenhagen reading vanished as a live interpretive option, textbook structures would reorganize around decoherence, pilot-wave, or Everettian frameworks; the epistemic license to dismiss the measurement problem would dissolve; career paths and funding would redistribute toward realist programs; and the boundary between physics and philosophy of physics would shift.
% FOUNDING_PROBLEM: The failure of classical physics to account for atomic stability, discrete spectra, and wave-particle duality, combined with the mathematical success of the wavefunction formalism that initially lacked an agreed interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream practitioners attest the founding problem is solved because predictions succeed. Realist foundational researchers and historians of science (e.g., Beller, Cushing) attest the founding problemâwhat happens during measurementâremains unsolved and that the Copenhagen reading is a sociologically contingent workaround rather than a unique solution compelled by evidence.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate-to-high because the reading extracts research capacity from realist programs by treating measurement as primitive; suppression (0.68) reflects historically strong institutional gatekeeping that has softened but persists in textbook and funding structures. Theater ratio (0.42) captures the elaborate philosophical superstructure (complementarity, correspondence principle) that maintains a pragmatic calculational recipe. Accessibility collapse is only 0.45 because sibling readings (MWI, pilot-wave, decoherence) remain widely accessible and actively developed. Resistance (0.58) is substantial due to ongoing foundational criticism and alternative programs.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat the reading appears as indispensable coordination: without the measurement postulate as primitive, the calculational framework fragments. From the target seat the same structure appears as active extraction: the primitivity claim suppresses legitimate research questions and redirects funding toward application and away from foundations. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream practitioners are structural beneficiaries: the reading gives them a shared calculational language without requiring ontological consensus, so their directionality sits near the beneficiary end. Realist foundational researchers are structural targets: they bear the costs of marginalization and must build parallel institutions, so their directionality sits near the target end. The pedagogical authority is the agenda setter that modulates enforcement; its exit options (arbitrage) reflect its capacity to shift curricula, though inertia makes this costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents misreading the Copenhagen reading as either a pure Mountain (natural law) or a pure Rope (coordination without cost). The strong ontological claimsâmeasurement as primitive, indeterminism as irreducibleâlend it a Mountain-like appearance, but the contested kernel context, persistent alternatives, and institutional enforcement pattern place it in Tangled Rope. It has a genuine coordination function (unified calculational practice) and asymmetric extraction (marginalization of realists), and it requires active enforcement (textbook canon, peer-review norms) to maintain the boundary against accessible alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_empirical_underdetermination,
    'Does the Copenhagen reading''s classification change if empirical underdetermination is resolved by a future experiment (e.g., decisive test of collapse models or a Bell-type inequality that eliminates all but one reading)?',
    'Experimental detection of wavefunction-collapse dynamics, or an empirically decisive test that distinguishes the Copenhagen reading''s stochastic boundary from MWI''s branching or pilot-wave''s determinism.',
    'If Copenhagen predictions are empirically distinguished from siblings, the reading could shift toward Mountain status; if falsified, it would collapse toward Piton or dissolve entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_empirical_underdetermination, empirical, 'Empirical underdetermination of the Copenhagen reading').

omega_variable(
    measurement_primitivity_status,
    'Is measurement primitivity a feature of nature or a methodological convenience that has become institutionally frozen?',
    'Comparative pedagogical and research outcomes: if training regimes that treat measurement as derived (decoherence-first or pilot-wave-first) produce equivalent or superior predictive success, primitivity is conventional rather than natural.',
    'If conventional, the constraint''s extractiveness is higher than if natural, because the coordination function (calculational recipe) is separable from the ontological boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_primitivity_status, conceptual, 'Status of measurement postulate as natural or conventional').

omega_variable(
    suppression_mechanism_interpretive,
    'Is the suppression of realist alternatives structural (institutional gatekeeping in funding, hiring, and publishing) or internalized (practitioners self-censoring foundational questions as ''not physics'')?',
    'Attitudinal surveys of physicists combined with institutional funding-flow analysis; if suppression persists after explicit institutional barriers are removed, it is internalized.',
    'Internalized suppression raises effective extraction beyond the structural measure; structural suppression confirms active enforcement characteristic of Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_interpretive, empirical, 'Structural vs internalized suppression in interpretive practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 98).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(quan_tr_t20, quantum_formalism__copenhagen_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(quan_tr_t45, quantum_formalism__copenhagen_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(quan_tr_t70, quantum_formalism__copenhagen_reading, theater_ratio, 70, 0.5).
narrative_ontology:measurement(quan_tr_t85, quantum_formalism__copenhagen_reading, theater_ratio, 85, 0.48).
narrative_ontology:measurement(quan_tr_t98, quantum_formalism__copenhagen_reading, theater_ratio, 98, 0.42).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(quan_be_t20, quantum_formalism__copenhagen_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(quan_be_t45, quantum_formalism__copenhagen_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(quan_be_t70, quantum_formalism__copenhagen_reading, base_extractiveness, 70, 0.6).
narrative_ontology:measurement(quan_be_t85, quantum_formalism__copenhagen_reading, base_extractiveness, 85, 0.58).
narrative_ontology:measurement(quan_be_t98, quantum_formalism__copenhagen_reading, base_extractiveness, 98, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__copenhagen_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(quan_su_t20, quantum_formalism__copenhagen_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(quan_su_t45, quantum_formalism__copenhagen_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(quan_su_t70, quantum_formalism__copenhagen_reading, suppression_requirement, 70, 0.6).
narrative_ontology:measurement(quan_su_t85, quantum_formalism__copenhagen_reading, suppression_requirement, 85, 0.58).
narrative_ontology:measurement(quan_su_t98, quantum_formalism__copenhagen_reading, suppression_requirement, 98, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The quantum_formalism kernel decomposes into at least three structurally distinct constraints (readings). The Copenhagen reading claims measurement primitivity and irreducible indeterminism; the many-worlds reading derives measurement from decoherence and eliminates indeterminism at the universal level; the pilot-wave reading restores determinism via hidden variables. These are not the same constraint viewed from different anglesâtheir epsilon values, victim sets, and enforcement patterns differ. They form a constraint family linked by mutual foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
