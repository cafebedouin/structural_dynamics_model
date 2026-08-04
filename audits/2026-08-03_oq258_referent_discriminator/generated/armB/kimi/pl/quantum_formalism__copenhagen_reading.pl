% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Interpretation of Quantum Mechanics
 *   domain: philosophy of physics / quantum foundations / interpretive epistemology
 *
 * SUMMARY:
 *   The Copenhagen reading of the quantum formalism treats wavefunction
 *   collapse as a physical process and measurement as an absolute,
 *   irreducible epistemic boundary. It emerged as the dominant interpretive
 *   framework in the 1930s, providing operational clarity while suppressing
 *   realist alternatives. This constraint story treats the Copenhagen reading
 *   as a commitment-system constraint: an interpretive tradition grounding
 *   its legitimacy in the quantum formalism kernel, maintained through
 *   institutional enforcement and pedagogical reproduction. The claim/metric
 *   independence is preserved: the reading is claimed as tangled_rope
 *   (genuine coordination plus asymmetric extraction) while the metrics
 *   capture substantial, historically variable extraction.
 *
 * KEY AGENTS:
 *   - copenhagen_orthodoxy: agenda_setter (institutional/constrained) — maintains interpretive authority
 *   - operational_physicists: beneficiary (organized/mobile) — uses framework without ontological burden
 *   - alternative_interpretation_researchers: payer (moderate/constrained) — bears exclusion from mainstream
 *   - graduate_students: payer (powerless/identity_locked) — absorb orthodoxy as default ontology
 *   - philosophy_of_physics_observers: observer (analytical) — traces interpretive structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.62).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.68).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Interpretation of Quantum Mechanics").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy of physics / quantum foundations / interpretive epistemology").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, 'bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8').
narrative_ontology:cs_kernel_codification('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', fixed_text).
narrative_ontology:cs_authority_grounding('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', lineage).
narrative_ontology:cs_interpretation_layer_present('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8').
narrative_ontology:cs_reading_relation('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', foundational, measurement_primitive_boundary).
narrative_ontology:cs_axiom_status(measurement_primitive_boundary, holdable).
narrative_ontology:cs_axiom_grounding('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', measurement_primitive_boundary, conventional).
narrative_ontology:cs_axiom('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', foundational, collapse_physical_process).
narrative_ontology:cs_axiom_status(collapse_physical_process, holdable).
narrative_ontology:cs_axiom_grounding('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', collapse_physical_process, empirically_contingent).
narrative_ontology:cs_reference_frame('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', classical_measurement_closure).
narrative_ontology:cs_drift_state('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', post_bell_inequality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bb2bbfa4-3782-4e4f-86c0-3a8c3668e8a8', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_orthodoxy).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, operational_physicists).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, graduate_students).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_irreducibility_thesis).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, observer_role_non_eliminable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains interpretive authority over quantum mechanics through peer review gatekeeping, curriculum standardization, and funding allocation. Accrues epistemic prestige and institutional control by treating measurement as a primitive boundary that halts further ontological inquiry. Exit is constrained because professional identity and institutional standing are fused to the orthodox framework.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_orthodoxy, agenda_setter,
    institutional, generational, constrained, global).

% Uses the Copenhagen framework as a calculational tool without engaging the measurement problem. Benefits from a standardized pedagogy and shared operational rules that allow prediction without metaphysical commitment. Can move to alternative interpretive frameworks individually but rarely do so because the orthodox framework is socially dominant and empirically adequate.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, operational_physicists, beneficiary,
    organized, biographical, mobile, global).

% Develop Bohmian mechanics, spontaneous collapse theories, and other realist interpretations. Bear costs in the form of exclusion from mainstream journals, reduced funding access, and marginalization in hiring decisions. Their exit options are constrained because the field's gatekeeping institutions are organized around the Copenhagen framework.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers, payer,
    moderate, biographical, constrained, global).

% Are trained into the Copenhagen framework as the default ontology of quantum mechanics. Learn to treat measurement as primitive and collapse as unanalyzable. Their professional identity forms around the orthodox formalism, making exit to alternative interpretations cognitively and socially costly. They pay through suppressed ontological curiosity and path dependence.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, graduate_students, payer,
    powerless, biographical, identity_locked, national).

% Trace the historical and structural dynamics of quantum interpretation debates. Do not collect from or pay into the constraint. Map the relationship between the formalism and its interpretive layers, documenting how the Copenhagen reading achieved and maintains dominance.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophy_of_physics_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, copenhagen_orthodoxy).
narrative_ontology:fixing_cost_class(quantum_formalism__copenhagen_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified operational framework for quantum mechanics that allows physicists to calculate measurement outcomes without resolving the ontology of measurement or the nature of the observer.
% TRANSFER_FUNCTION: Moves epistemic authority and career resources from foundational dissenters to the orthodox community; enforces a methodological norm that treats measurement as a primitive terminative boundary.
% ABSENT_VOICES: Bohmian mechanics researchers, spontaneous collapse theorists, many-worlds advocates, and realist-inclined graduate students are structurally underrepresented in mainstream funding bodies, textbook committees, and tenure lines.
% DISAPPEARANCE_RATIONALE: The constraint organizes a large fraction of physics pedagogy and practice around a specific interpretive silence; its disappearance would force explicit engagement with the measurement problem, redistribute epistemic authority across interpretations, and alter graduate training curricula.
% FOUNDING_PROBLEM: The quantum formalism yielded correct predictions but apparent paradoxes (wave-particle duality, measurement problem); physicists needed a way to use the mathematics without resolving its ontological implications.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary quantum foundations researchers (e.g., Bell, Deutsch, and modern Bohmian and decoherence theorists) attest from outside the benefiting orthodoxy that the measurement problem remains live and that Copenhagen silence was a methodological expedient, not a resolution. Mainstream operational physicists attest the problem is solved by the operational protocol.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the substantial epistemic extraction from suppressed research programs and locked-in graduate training. Suppression (0.68) captures active historical enforcement: Bohmian mechanics was excluded from textbooks and tenure lines for decades. Theater ratio (0.45) reflects that a growing share of maintenance activity is performative: the 'shut up and calculate' norm persists after alternative interpretations demonstrate equivalent empirical coverage. Accessibility collapse (0.60) indicates that while alternatives exist in principle, the pedagogical and funding landscape makes them difficult to access. Resistance (0.55) measures the pushback from quantum foundations revival since Bell. Temporal measurements trace a lifecycle: extraction and suppression peaked mid-century (T=45-60) and have modestly declined as quantum information and foundations research eroded the authority structure, though theater has risen to maintain the orthodoxy inertially.
 *
 * PERSPECTIVAL GAP:
 *   Operational physicists experience a streamlined calculational tool; dissenting researchers experience exclusion from funding and publication; graduate students experience identity-lock as they are trained to treat measurement as primitive and unanalyzable. The orthodox agenda-setter experiences a legitimate framework; the analytical observer sees the enforcement mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   copenhagen_orthodoxy and operational_physicists are beneficiaries with mobile or constrained exit, yielding low directionality (subsidy or mild cost). alternative_interpretation_researchers and graduate_students are declared victims with constrained or identity_locked exit, yielding high directionality (amplified extraction). The effective extraction chi is thus concentrated on the trapped and identity-locked agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling physics to proceed despite interpretive paradox — was genuinely live in 1930. By 1960, Bohmian mechanics and later decoherence showed that operational success does not require Copenhagen ontology. The founding problem status is contested: operational physicists claim the framework is still necessary for pedagogy, while foundations researchers claim alternatives suffice. This mismatch signals mandatrophy risk: the arrangement persists partly because it solves a coordination problem (common language) and partly because the orthodoxy extracts authority from its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copenhagen_vs_formalism_necessity,
    'Is the Copenhagen interpretation structurally necessitated by the quantum formalism, or is it an optional interpretive layer detachable from the mathematical kernel?',
    'Demonstrating a complete realist interpretation (pilot wave or many-worlds) with equivalent empirical coverage resolves that the kernel does not necessitate Copenhagen.',
    'If detachable, the constraint is an imposed interpretation (higher extraction); if necessitated, it approaches a Mountain (natural law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copenhagen_vs_formalism_necessity, conceptual, 'Whether Copenhagen is intrinsic to the formalism or a parasitic interpretive layer.').

omega_variable(
    enforcement_social_or_empirical,
    'Does the persistence of the Copenhagen reading depend on social enforcement within physics institutions, or would it remain dominant through pure empirical adequacy?',
    'Historical counterfactual analysis of Bohmian mechanics suppression in the 1950s-70s; natural experiment from regions with stronger foundational traditions.',
    'If enforcement-dependent, confirms tangled_rope dynamics; if empirically self-selecting, suggests rope or mountain characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_social_or_empirical, empirical, 'Whether dominance is socially enforced or empirically selected.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(quan_tr_t15, quantum_formalism__copenhagen_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(quan_tr_t30, quantum_formalism__copenhagen_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(quan_tr_t45, quantum_formalism__copenhagen_reading, theater_ratio, 45, 0.32).
narrative_ontology:measurement(quan_tr_t60, quantum_formalism__copenhagen_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(quan_tr_t75, quantum_formalism__copenhagen_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement(quan_tr_t90, quantum_formalism__copenhagen_reading, theater_ratio, 90, 0.45).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(quan_be_t15, quantum_formalism__copenhagen_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(quan_be_t30, quantum_formalism__copenhagen_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(quan_be_t45, quantum_formalism__copenhagen_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(quan_be_t60, quantum_formalism__copenhagen_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(quan_be_t75, quantum_formalism__copenhagen_reading, base_extractiveness, 75, 0.7).
narrative_ontology:measurement(quan_be_t90, quantum_formalism__copenhagen_reading, base_extractiveness, 90, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantum_formalism__copenhagen_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(quan_su_t15, quantum_formalism__copenhagen_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(quan_su_t30, quantum_formalism__copenhagen_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(quan_su_t45, quantum_formalism__copenhagen_reading, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(quan_su_t60, quantum_formalism__copenhagen_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(quan_su_t75, quantum_formalism__copenhagen_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(quan_su_t90, quantum_formalism__copenhagen_reading, suppression_requirement, 90, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The quantum_formalism kernel decomposes into multiple structurally distinct constraints. This story isolates the Copenhagen reading (measurement primitive, irreducible indeterminism). Sibling constraints address the many-worlds reading (deterministic branching) and pilot-wave reading (deterministic hidden variables). These are not observational variants of one constraint; they have different ontological commitments, different empirical interpretations of the same formalism, and different institutional effects. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
