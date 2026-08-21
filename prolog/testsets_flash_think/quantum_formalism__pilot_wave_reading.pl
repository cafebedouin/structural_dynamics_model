% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Pilot-Wave Interpretation of Quantum Mechanics
 *   domain: philosophy_of_physics/quantum_foundations/interpretive_epistemology
 *
 * SUMMARY:
 *   This constraint story instantiates the pilot-wave reading of the quantum
 *   formalism kernel. It describes the interpretation where particles have
 *   definite positions guided by a pilot wave (the wavefunction as a physical
 *   field), and deterministic hidden variables restore a classical ontology.
 *   This reading is a minority view in quantum foundations, offering
 *   conceptual benefits to its adherents but facing significant intellectual
 *   and institutional resistance from mainstream interpretations. The claimed
 *   type is 'tangled_rope' because it provides a genuine conceptual
 *   coordination function (a coherent, deterministic ontology) but also
 *   imposes asymmetric extraction in terms of career costs and suppression
 *   for its proponents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.65).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.75).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot-Wave Interpretation of Quantum Mechanics").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations/interpretive_epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '37c12579-ff1a-48ef-b69f-c9123e529586').
narrative_ontology:cs_kernel_codification('37c12579-ff1a-48ef-b69f-c9123e529586', formalized).
narrative_ontology:cs_authority_grounding('37c12579-ff1a-48ef-b69f-c9123e529586', expertise).
narrative_ontology:cs_interpretation_layer_present('37c12579-ff1a-48ef-b69f-c9123e529586').
narrative_ontology:cs_reading_relation('37c12579-ff1a-48ef-b69f-c9123e529586', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('37c12579-ff1a-48ef-b69f-c9123e529586', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_axiom('37c12579-ff1a-48ef-b69f-c9123e529586', foundational, particles_have_definite_positions_at_all_times).
narrative_ontology:cs_axiom_status(particles_have_definite_positions_at_all_times, holdable).
narrative_ontology:cs_axiom_grounding('37c12579-ff1a-48ef-b69f-c9123e529586', particles_have_definite_positions_at_all_times, deontological).
narrative_ontology:cs_axiom('37c12579-ff1a-48ef-b69f-c9123e529586', foundational, wavefunction_is_physical_pilot_field).
narrative_ontology:cs_axiom_status(wavefunction_is_physical_pilot_field, holdable).
narrative_ontology:cs_axiom_grounding('37c12579-ff1a-48ef-b69f-c9123e529586', wavefunction_is_physical_pilot_field, empirically_contingent).
narrative_ontology:cs_reference_frame('37c12579-ff1a-48ef-b69f-c9123e529586', classical_deterministic_reality).
narrative_ontology:cs_drift_state('37c12579-ff1a-48ef-b69f-c9123e529586', contemporary_quantum_foundations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('37c12579-ff1a-48ef-b69f-c9123e529586', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, pilot_wave_proponents).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, classical_realists).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, philosophers_of_physics).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, mainstream_quantum_physicists).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, students_of_quantum_mechanics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively research, publish, and advocate for the pilot-wave interpretation. They gain conceptual clarity and a deterministic, realist worldview, but face career challenges and funding difficulties due to its non-mainstream status.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, pilot_wave_proponents, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from an interpretation that restores classical notions of determinism and objective reality, aligning with their philosophical predispositions. They are not directly involved in research but find intellectual satisfaction.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, classical_realists, beneficiary,
    moderate, biographical, mobile, global).

% Find rich intellectual ground in the pilot-wave interpretation, using it to explore foundational questions of quantum mechanics, causality, and realism. They benefit from the conceptual clarity it offers to these debates.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, philosophers_of_physics, beneficiary,
    analytical, generational, analytical, universal).

% Primarily work within the Copenhagen or Many-Worlds frameworks. They bear the intellectual cost of engaging with or defending against the pilot-wave interpretation, which challenges their established paradigms, though they can largely ignore it in their daily work.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, mainstream_quantum_physicists, payer,
    institutional, biographical, mobile, global).

% Encounter a confusing landscape of interpretations, with pilot-wave theory presenting a compelling but non-standard view. They bear the cost of navigating this lack of consensus and the potential for intellectual dissonance.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, students_of_quantum_mechanics, payer,
    powerless, immediate, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__pilot_wave_reading, pilot_wave_proponents).
narrative_ontology:fixing_cost_class(quantum_formalism__pilot_wave_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent, deterministic, and realist interpretation of quantum mechanics, resolving the measurement problem and non-locality paradoxes within a classical ontology, thereby coordinating conceptual understanding for its adherents.
% TRANSFER_FUNCTION: Transfers conceptual clarity and a classical intuition about reality to its proponents, while imposing intellectual burden, career risk, and a sense of non-conformity on those who adhere to it within the broader physics community.
% ABSENT_VOICES: Physicists and philosophers who adhere to interpretations that posit irreducible indeterminism (e.g., Copenhagen) or universal branching (e.g., Many-Worlds) are conceptually excluded from the pilot-wave narrative of restored classical ontology.
% DISAPPEARANCE_RATIONALE: If the pilot-wave interpretation vanished overnight, the landscape of quantum foundations would simplify, but the fundamental conceptual problems it addresses (measurement, non-locality, realism) would remain unresolved within a deterministic, classical framework, forcing other interpretations to fill the void or leaving a significant philosophical gap.
% FOUNDING_PROBLEM: The measurement problem, wave-particle duality, and non-locality in standard quantum mechanics, leading to a lack of a clear, deterministic, realist ontology and persistent conceptual unease.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of pilot-wave theory and a significant minority of philosophers of physics attest to the ongoing nature of these foundational problems. While mainstream physicists often consider these 'solved' or 'non-problems,' the philosophical community largely corroborates their persistence, providing external validation for the problem's 'live' status.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__pilot_wave_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantum_formalism__pilot_wave_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) reflects the intellectual and career costs borne by pilot-wave proponents due to its non-mainstream status, despite the conceptual benefits it offers. Suppression (0.75) is high, indicating the strong institutional and intellectual resistance it faces from dominant interpretations. The theater ratio is low (0.10) because it is a serious, active research program, not merely performative. Accessibility collapse is moderate (0.40) as other interpretations are dominant but pilot-wave remains a recognized, if minority, alternative. Resistance (0.55) is moderate, reflecting ongoing intellectual debate and advocacy from its proponents against mainstream views.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pilot-wave proponents, this interpretation is a 'rope' or even a 'mountain' of conceptual truth, offering a clear and consistent understanding of reality. From the perspective of mainstream physicists, it might be seen as a 'snare' that distracts from more fruitful research, or a 'piton' of a bygone era. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Pilot-wave proponents, classical realists, and philosophers of physics are beneficiaries, gaining conceptual clarity and a preferred worldview. Mainstream quantum physicists and students of quantum mechanics are victims, bearing the intellectual burden of engagement or the confusion of a non-consensus view. The 'agenda_setter' role for pilot-wave proponents reflects their active advocacy, despite facing institutional headwinds.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_vs_ontological_extraction,
    'Is the ''extraction'' experienced by pilot-wave proponents primarily epistemic (difficulty in publication, funding) or ontological (the intellectual burden of holding a non-consensus view)?',
    'Sociological studies of funding and publication rates for pilot-wave research vs. mainstream, combined with qualitative interviews on intellectual satisfaction/dissonance.',
    'If primarily epistemic, the constraint is more clearly a ''snare'' of institutional power. If primarily ontological, the ''tangled_rope'' classification is more robust, reflecting a trade-off for conceptual benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_vs_ontological_extraction, empirical, 'Distinguishing the nature of extraction for non-mainstream scientific views.').

omega_variable(
    empirical_distinguishability,
    'Can the pilot-wave interpretation ever be empirically distinguished from other interpretations (e.g., Copenhagen or Many-Worlds)?',
    'Development of novel experimental tests that yield different predictions for pilot-wave theory compared to other interpretations, or a theoretical proof of empirical equivalence.',
    'If empirically distinguishable and confirmed, its status would shift towards a ''mountain'' of physical fact. If proven empirically equivalent, its persistence remains a ''preference'' or ''conceptual'' choice, reinforcing its current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_distinguishability, empirical, 'The potential for experimental evidence to resolve interpretive debates.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of funding, publication bias) or internalized (proponents self-censor or avoid certain topics)?',
    'Analysis of funding success rates and publication acceptance rates for pilot-wave research, combined with surveys of researchers regarding self-censorship or perceived barriers.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as proponents carry the suppression with them. If structural, it points to institutional biases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-mainstream scientific theories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 1952, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1952, quantum_formalism__pilot_wave_reading, theater_ratio, 1952, 0.1).
narrative_ontology:measurement(quan_tr_t1970, quantum_formalism__pilot_wave_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(quan_tr_t1990, quantum_formalism__pilot_wave_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__pilot_wave_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(quan_tr_t2024, quantum_formalism__pilot_wave_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(quan_be_t1952, quantum_formalism__pilot_wave_reading, base_extractiveness, 1952, 0.6).
narrative_ontology:measurement(quan_be_t1970, quantum_formalism__pilot_wave_reading, base_extractiveness, 1970, 0.62).
narrative_ontology:measurement(quan_be_t1990, quantum_formalism__pilot_wave_reading, base_extractiveness, 1990, 0.64).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__pilot_wave_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(quan_be_t2024, quantum_formalism__pilot_wave_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1952, quantum_formalism__pilot_wave_reading, suppression_requirement, 1952, 0.7).
narrative_ontology:measurement(quan_su_t1970, quantum_formalism__pilot_wave_reading, suppression_requirement, 1970, 0.72).
narrative_ontology:measurement(quan_su_t1990, quantum_formalism__pilot_wave_reading, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(quan_su_t2010, quantum_formalism__pilot_wave_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(quan_su_t2024, quantum_formalism__pilot_wave_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, information_standard).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quantum_formalism' kernel, alongside the Copenhagen and Many-Worlds interpretations. Each offers a distinct ontological and epistemological framework for quantum mechanics, with differing implications for determinism, realism, and the role of the observer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
