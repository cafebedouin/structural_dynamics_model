% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Copenhagen Interpretation of Quantum Mechanics
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   The Copenhagen interpretation of quantum mechanics posits that
 *   wavefunction collapse is a physical process triggered by measurement,
 *   leading to irreducible indeterminism and an absolute epistemic boundary.
 *   This constraint represents the dominant, historically entrenched reading
 *   of quantum formalism, which emphasizes operational utility over
 *   ontological completeness. It is presented as a fundamental truth of
 *   physics, but its conceptual implications are highly contested by
 *   alternative interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.68).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.75).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, mountain).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Interpretation of Quantum Mechanics").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).
domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '4a723428-fdd2-4797-95a0-a5187ecf7c07').
narrative_ontology:cs_kernel_codification('4a723428-fdd2-4797-95a0-a5187ecf7c07', formalized).
narrative_ontology:cs_authority_grounding('4a723428-fdd2-4797-95a0-a5187ecf7c07', practice).
narrative_ontology:cs_interpretation_layer_present('4a723428-fdd2-4797-95a0-a5187ecf7c07').
narrative_ontology:cs_reading_relation('4a723428-fdd2-4797-95a0-a5187ecf7c07', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_reading_relation('4a723428-fdd2-4797-95a0-a5187ecf7c07', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('4a723428-fdd2-4797-95a0-a5187ecf7c07', foundational, measurement_as_primitive_indeterministic_act).
narrative_ontology:cs_axiom_status(measurement_as_primitive_indeterministic_act, holdable).
narrative_ontology:cs_axiom_grounding('4a723428-fdd2-4797-95a0-a5187ecf7c07', measurement_as_primitive_indeterministic_act, conventional).
narrative_ontology:cs_axiom('4a723428-fdd2-4797-95a0-a5187ecf7c07', secondary, wavefunction_not_ontologically_real).
narrative_ontology:cs_axiom_status(wavefunction_not_ontologically_real, holdable).
narrative_ontology:cs_axiom_grounding('4a723428-fdd2-4797-95a0-a5187ecf7c07', wavefunction_not_ontologically_real, conventional).
narrative_ontology:cs_reference_frame('4a723428-fdd2-4797-95a0-a5187ecf7c07', quantum_formalism_as_epistemic_tool).
narrative_ontology:cs_drift_state('4a723428-fdd2-4797-95a0-a5187ecf7c07', contemporary_quantum_foundations_debate, gap(stable, minor, false)).
narrative_ontology:cs_created_at('4a723428-fdd2-4797-95a0-a5187ecf7c07', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_proponents).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, experimental_physicists).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, realist_theorists).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, determinists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and teaches the Copenhagen interpretation as the standard view, benefiting from its conceptual clarity for calculation and its historical dominance. Their professional identity is often tied to this framework.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_proponents, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the 'shut up and calculate' ethos, which allows them to focus on empirical results without getting bogged down in deep ontological debates. They accept the probabilistic nature of measurement as a given.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, experimental_physicists, beneficiary,
    organized, biographical, constrained, global).

% Bear the cost of having to abandon classical notions of objective reality and local determinism. They seek deeper, more intuitive explanations for quantum phenomena, often finding Copenhagen's 'primitive' measurement unsatisfactory.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, realist_theorists, payer,
    powerful, generational, constrained, global).

% Are fundamentally at odds with the irreducible indeterminism posited by Copenhagen. They are 'trapped' by the framework's insistence on non-deterministic outcomes, which challenges a core philosophical commitment.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, determinists, payer,
    powerless, civilizational, trapped, universal).

% Propose an alternative interpretation that eliminates collapse and restores determinism through universal wavefunction evolution and branching worlds. Their view is often marginalized or dismissed by Copenhagen proponents.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, many_worlds_advocates, excluded,
    organized, generational, constrained, global).

% Propose a deterministic hidden-variable theory with particles guided by a pilot wave. This interpretation is also often excluded from mainstream discussion by Copenhagen proponents due to its non-local nature and classical ontology.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, pilot_wave_advocates, excluded,
    organized, generational, constrained, global).

% Critically analyze the conceptual foundations and implications of the Copenhagen interpretation, comparing it with alternatives and exploring its philosophical coherence. They are not bound by its operational dictates.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophers_of_physics, observer,
    analytical, civilizational, analytical, universal).

narrative_ontology:fixing_cost_class(quantum_formalism__copenhagen_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent and widely accepted framework for interpreting the mathematical formalism of quantum mechanics, particularly regarding measurement outcomes and probabilities, enabling physicists to 'shut up and calculate' and communicate results effectively.
% TRANSFER_FUNCTION: Transfers the burden of ontological explanation from the quantum formalism itself to the act of measurement, and transfers epistemic certainty from classical determinism to irreducible probability. It also transfers intellectual capital and institutional support to its proponents.
% ABSENT_VOICES: Advocates of alternative interpretations (Many-Worlds, Pilot-Wave) are often marginalized in mainstream physics education and discourse, despite offering coherent, deterministic, or realist alternatives. They would argue for a more complete ontological picture.
% DISAPPEARANCE_RATIONALE: If the Copenhagen interpretation vanished overnight, the foundational conceptual framework for understanding quantum mechanics would be lost. Physicists would lack a common language and set of assumptions for interpreting experiments, leading to a profound reorganization of research, teaching, and philosophical inquiry in quantum foundations.
% FOUNDING_PROBLEM: To provide a coherent and empirically consistent interpretation of quantum mechanics that accounts for the probabilistic nature of measurement outcomes and the apparent discontinuity of wavefunction collapse, without resorting to unobservable hidden variables or classical intuitions.
% FOUNDING_PROBLEM_CORROBORATION: The problem of interpreting quantum mechanics remains live, as evidenced by ongoing debates in quantum foundations and the development of new interpretations. While Copenhagen provides an operational solution, its conceptual completeness is still contested by philosophers and some physicists outside the core proponent group.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, ExtMetricName, E),
    domain_priors:suppression_score(quantum_formalism__copenhagen_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quantum_formalism__copenhagen_reading),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high because it demands physicists abandon classical intuitions about determinism and objective reality, imposing a significant conceptual cost on those seeking a realist ontology. `suppression` is high due to the historical and institutional dominance of Copenhagen, which has often marginalized alternative interpretations in mainstream discourse and education. `theater_ratio` is low as the interpretation is genuinely functional for calculation. `accessibility_collapse` is high because, for those who accept Copenhagen, alternative interpretations are often seen as unnecessary or conceptually flawed. `resistance` is high, reflecting the ongoing, vigorous debate in quantum foundations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Copenhagen proponents, this interpretation is a necessary and natural consequence of quantum mechanics, a 'mountain' that simply describes reality. From the perspective of realist theorists and determinists, it is an extractive 'snare' that forces them to abandon cherished scientific principles without offering a complete ontological picture. The engine's FSM detection will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Copenhagen proponents and experimental physicists benefit from the operational clarity and calculational power it provides, allowing them to focus on empirical results. Realist theorists and determinists bear the cost of its anti-realist and indeterministic tenets, which challenge their core philosophical commitments. Alternative interpretation advocates (Many-Worlds, Pilot-Wave) are structurally excluded from the mainstream discourse that Copenhagen dominates.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observer_role_ambiguity,
    'Is the ''observer'' in wavefunction collapse a conscious entity, a macroscopic measuring device, or merely an interaction with an environment?',
    'Development of a universally accepted, precise definition of ''measurement'' or ''observer'' within the Copenhagen framework that eliminates ambiguity without resorting to external interpretations.',
    'If the observer''s role is clarified to be purely physical (e.g., decoherence), it might reduce the perceived ''epistemic boundary'' and lower extractiveness for realists. If it remains tied to consciousness, it increases the conceptual cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_role_ambiguity, conceptual, 'Ambiguity in the definition and role of the observer in quantum measurement.').

omega_variable(
    indeterminism_fundamentality,
    'Is the indeterminism observed in quantum mechanics truly irreducible, or is it a consequence of an incomplete description (e.g., hidden variables)?',
    'Empirical discovery of hidden variables that restore determinism, or a rigorous proof that no such variables can exist consistent with quantum mechanics.',
    'If determinism is restored (e.g., via hidden variables), the core ''indeterminism'' axiom of Copenhagen would be overridden, fundamentally altering its classification and reducing extraction for determinists. If proven irreducible, Copenhagen''s claim to naturalness is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indeterminism_fundamentality, empirical, 'The fundamental nature of quantum indeterminism.').

omega_variable(
    collapse_process_physicality,
    'Is wavefunction collapse a genuine physical process, or an artifact of our epistemic limitations in describing a deterministic underlying reality?',
    'Direct experimental observation of the collapse process itself, or a theoretical framework that fully describes the transition from superposition to definite state without invoking a primitive collapse postulate.',
    'If collapse is shown to be an emergent phenomenon from a deterministic underlying theory (e.g., Many-Worlds or Pilot-Wave), Copenhagen''s claim of a primitive physical collapse would be overridden, reducing its perceived naturalness and increasing resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_process_physicality, empirical, 'The physical reality vs. epistemic nature of wavefunction collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1927, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1927, quantum_formalism__copenhagen_reading, theater_ratio, 1927, 0.05).
narrative_ontology:measurement(quan_tr_t1947, quantum_formalism__copenhagen_reading, theater_ratio, 1947, 0.08).
narrative_ontology:measurement(quan_tr_t1967, quantum_formalism__copenhagen_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(quan_tr_t1987, quantum_formalism__copenhagen_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(quan_tr_t2007, quantum_formalism__copenhagen_reading, theater_ratio, 2007, 0.1).
narrative_ontology:measurement(quan_tr_t2024, quantum_formalism__copenhagen_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.5).
narrative_ontology:measurement(quan_be_t1947, quantum_formalism__copenhagen_reading, base_extractiveness, 1947, 0.6).
narrative_ontology:measurement(quan_be_t1967, quantum_formalism__copenhagen_reading, base_extractiveness, 1967, 0.65).
narrative_ontology:measurement(quan_be_t1987, quantum_formalism__copenhagen_reading, base_extractiveness, 1987, 0.68).
narrative_ontology:measurement(quan_be_t2007, quantum_formalism__copenhagen_reading, base_extractiveness, 2007, 0.67).
narrative_ontology:measurement(quan_be_t2024, quantum_formalism__copenhagen_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1927, quantum_formalism__copenhagen_reading, suppression_requirement, 1927, 0.4).
narrative_ontology:measurement(quan_su_t1947, quantum_formalism__copenhagen_reading, suppression_requirement, 1947, 0.6).
narrative_ontology:measurement(quan_su_t1967, quantum_formalism__copenhagen_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(quan_su_t1987, quantum_formalism__copenhagen_reading, suppression_requirement, 1987, 0.75).
narrative_ontology:measurement(quan_su_t2007, quantum_formalism__copenhagen_reading, suppression_requirement, 2007, 0.73).
narrative_ontology:measurement(quan_su_t2024, quantum_formalism__copenhagen_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
