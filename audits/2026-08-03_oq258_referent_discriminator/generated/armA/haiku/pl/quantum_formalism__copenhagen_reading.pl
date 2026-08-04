% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism_copenhagen_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Copenhagen Interpretation: Wavefunction Collapse as Physical Process
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The Copenhagen interpretation of quantum mechanics treats wavefunction
 *   collapse as a physical process occurring at measurement, measurement as a
 *   primitive ontological category, and the observer role as non-eliminable
 *   from the theory's foundational structure. This is ONE reading of the
 *   quantum formalism kernel — a stabilized formal commitment (the
 *   Schrödinger equation, the Born rule, the measurement postulate) that
 *   different authority structures interpret differently. The Copenhagen
 *   reading institutionalized this interpretation in standard textbooks and
 *   pedagogy, treating it as the canonical reading. Alternative readings
 *   (many-worlds, pilot-wave, objective collapse) solve the measurement
 *   problem differently, restoring determinism or realism or both. The
 *   constraint extracts institutional authority for Copenhagen and imposes
 *   costs on researchers pursuing alternatives.
 *
 * KEY AGENTS:
 *   - measurement_foundationalism_research_program: Institutional agenda-setter controlling pedagogical standards and journal gatekeeping
 *   - deterministic_interpretation_advocates: Moderate-power researchers pursuing deterministic completions; systematically disadvantaged
 *   - realist_ontology_researchers: Powerful researchers pursuing mind-independent ontologies; higher institutional standing than determinists but still marginalized
 *   - practical_quantum_engineers: Beneficiaries of the coordinated formalism; exit option strong (use Copenhagen math without endorsing ontology)
 *   - foundational_physics_community: Observers who debate but do not settle the interpretation question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.68).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.71).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Interpretation: Wavefunction Collapse as Physical Process").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '53cd83e7-5982-4591-ae7e-1317e5203ad5').
narrative_ontology:cs_kernel_codification('53cd83e7-5982-4591-ae7e-1317e5203ad5', fixed_text).
narrative_ontology:cs_authority_grounding('53cd83e7-5982-4591-ae7e-1317e5203ad5', extraction).
narrative_ontology:cs_interpretation_layer_present('53cd83e7-5982-4591-ae7e-1317e5203ad5').
narrative_ontology:cs_reading_relation('53cd83e7-5982-4591-ae7e-1317e5203ad5', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('53cd83e7-5982-4591-ae7e-1317e5203ad5', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('53cd83e7-5982-4591-ae7e-1317e5203ad5', foundational, wavefunction_collapse_is_physical).
narrative_ontology:cs_axiom_status(wavefunction_collapse_is_physical, holdable).
narrative_ontology:cs_axiom_grounding('53cd83e7-5982-4591-ae7e-1317e5203ad5', wavefunction_collapse_is_physical, empirically_contingent).
narrative_ontology:cs_axiom('53cd83e7-5982-4591-ae7e-1317e5203ad5', foundational, measurement_as_primitive_ontological_category).
narrative_ontology:cs_axiom_status(measurement_as_primitive_ontological_category, holdable).
narrative_ontology:cs_axiom_grounding('53cd83e7-5982-4591-ae7e-1317e5203ad5', measurement_as_primitive_ontological_category, deontological).
narrative_ontology:cs_axiom('53cd83e7-5982-4591-ae7e-1317e5203ad5', secondary, observer_role_non_eliminable).
narrative_ontology:cs_axiom_status(observer_role_non_eliminable, holdable).
narrative_ontology:cs_axiom_grounding('53cd83e7-5982-4591-ae7e-1317e5203ad5', observer_role_non_eliminable, instrumental).
narrative_ontology:cs_reference_frame('53cd83e7-5982-4591-ae7e-1317e5203ad5', measurement_indeterminism_framework).
narrative_ontology:cs_drift_state('53cd83e7-5982-4591-ae7e-1317e5203ad5', contemporary_quantum_foundations_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53cd83e7-5982-4591-ae7e-1317e5203ad5', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, measurement_foundationalism_research_program).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, deterministic_interpretation_advocates).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, realist_ontology_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, practical_quantum_engineers).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, observer_role_non_eliminable).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_as_primitive_ontological_category).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, absolute_epistemic_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the research agenda by treating measurement as a primitive, irreducible ontological category and wavefunction collapse as physical fact. Administers textbooks, pedagogical practice, journal editorial standards that defer to this framing. The program collects institutional legitimacy and career advancement for researchers working within this interpretation.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, measurement_foundationalism_research_program, agenda_setter,
    institutional, generational, arbitrage, global).

% Invest decades in research programs (pilot-wave, many-worlds, objective collapse) aimed at restoring determinism and eliminating the observer role. Face systematic publication disadvantages, pedagogical marginalization, and resource constraints because the Copenhagen reading dominates standard quantum mechanics teaching and journal gatekeeping. Cannot easily exit because quantum mechanics is foundational to their field.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, deterministic_interpretation_advocates, payer,
    moderate, biographical, constrained, global).

% Pursue interpretations grounding quantum mechanics in mind-independent physical reality (realism): hidden variables, universal wavefunction branches, particle-wave dualism. They bear a cost in pedagogical friction (the Copenhagen reading is taught as 'standard') and face implicit pressure in hiring/funding decisions favoring instrumentalist interpretations. Their exit option is stronger than deterministic advocates because senior realists have institutional standing.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, realist_ontology_researchers, payer,
    powerful, biographical, mobile, global).

% Use quantum mechanics as an engineering tool for computing predictions of measurement outcomes. The Copenhagen reading's practical formalism (Born rule, collapse, measurement basis selection) works for prediction without requiring commitment to determinism or hidden variables. They benefit from the ambiguity: use the math without specifying ontology.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, practical_quantum_engineers, beneficiary,
    powerful, biographical, arbitrage, global).

% Debates the interpretation question: whether the Copenhagen reading's ontological claims (collapse is real, observer role is primitive) are correct, or whether alternative interpretations better serve foundational physics. Stakes are high for the field's self-understanding, but the practical formalism works regardless.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, foundational_physics_community, observer,
    institutional, generational, analytical, global).

% Textbooks, pedagogical standards, and the canonical mathematical formalism in standard physics curricula are treated as neutral vehicles for teaching quantum mechanics. In fact, they encode the Copenhagen interpretive choices (measurement as primitive, collapse as rule, observer as gate to physical reality) as pedagogical defaults, making alternative readings appear like non-standard extensions rather than equally viable foundational approaches.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_foundational_texts, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quantum_formalism__copenhagen_reading, quantum_foundational_texts).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified mathematical-physical framework for quantum mechanics that avoids the interpretive problems of assigning ontological status to unmeasured states: the wavefunction is a tool for computing measurement probabilities, collapse is the mechanism that enforces consistency between state and outcome.
% TRANSFER_FUNCTION: Transfers conceptual and institutional authority from deterministic physical theory (classical mechanics, realism) to indeterministic measurement-based theory (Copenhagen). The authority flows to researchers and frameworks accepting measurement-as-primitive at the cost of abandoning classical determinism and observer-independent reality.
% ABSENT_VOICES: Researchers pursuing deterministic completions of quantum mechanics (pilot-wave, many-worlds, objective collapse) are systemically underrepresented in textbook standards and pedagogical canon. They would argue measurement is not primitive and wavefunction evolution is deterministic; they are kept out by the institutionalized Copenhagen consensus.
% DISAPPEARANCE_RATIONALE: Practical quantum engineering continues unchanged if Copenhagen collapse is replaced with decoherence (many-worlds) or hidden variables (pilot-wave). The computational predictions remain identical. But the foundational physics community's self-understanding would rearrange: determinism would be restored, the observer role would become eliminable, realism would be viable. The coordination benefit (unified mathematical framework) persists under all readings; the extraction (institutional privilege for one ontological stance) would dissolve.
% FOUNDING_PROBLEM: Early quantum mechanics (1920s–1930s) faced the measurement problem: unmeasured quantum systems evolve deterministically (Schrödinger equation), but measurement gives one outcome. How to reconcile deterministic evolution with random-appearing outcomes? The Copenhagen reading solves it: wavefunction describes only measurement statistics, collapse is physical, measurement is primitive.
% FOUNDING_PROBLEM_CORROBORATION: Copenhagen adherents argue the problem is live: alternative interpretations multiply unmeasured possibilities (many-worlds) or invoke invisible variables (pilot-wave), complicating physics unnecessarily. Deterministic interpretation advocates (Bohm, Wallace, Vaidman, GRW researchers) attest the founding problem has been SOLVED by their approaches: you can restore determinism, eliminate observer role, and preserve all predictions. Independent foundational physics analysis supports the diagnosis: the measurement problem admits multiple consistent solutions; none has been empirically ruled out.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The constraint is classified TANGLED ROPE, not pure extraction (snare). It COORDINATES a unified mathematical framework (beneficiary: practical quantum engineers, the measurement formalism that works) WHILE it EXTRACTS institutional authority by privileging one interpretation (victims: deterministic and realist researchers). Extractiveness climbs from 0.45 to 0.68 over the century as alternative interpretations develop and are systematically marginalized rather than engaged. Theater ratio rises (0.15 → 0.42) because increasingly, textbooks present Copenhagen as the 'standard' interpretation while downplaying that it is one reading among several, treating the measurement postulate as neutral formalism rather than interpretive choice. Suppression requirement rises (0.35 → 0.71) because maintaining the Copenhagen hegemony requires active enforcement: editorial resistance to alternative-interpretation papers, pedagogical gatekeeping (Einstein versus Bohr becomes canonical; Bohm and later determinists become footnotes), and implicit resource pressure favoring researchers who do not challenge the quantum-measurement-as-fundamental framework.
 *
 * PERSPECTIVAL GAP:
 *   From the measurement_foundationalism program's position, the constraint is genuine coordination: measurement is primitive, collapse is physical, the formalism works. From the deterministic advocates' position, it is pure extraction with false justification: determinism can be restored without changing any predictions (hidden variables, many-worlds), so the apparent necessity of accepting measurement-as-primitive and indeterminism is enforced, not natural. The realist researchers' position is intermediate: they accept that the formalism coordinates, but deny that its success proves the Copenhagen ontology. The engine computes this seat divergence from power + exit + victim/beneficiary data: victims with constrained exit and denied alternatives sit at the target end of directionality; the agenda-setter at the beneficiary end.
 *
 * DIRECTIONALITY LOGIC:
 *   measurement_foundationalism_research_program: d ≈ 0.05 (full beneficiary—controls the rules, collects institutional authority, could change the framework but has no reason to). deterministic_interpretation_advocates: d ≈ 0.85 (near-full target—constrained exit, no real alternative entrance into foundational physics without working within quantum formalism, which they are trying to complete). realist_ontology_researchers: d ≈ 0.55 (near-symmetric—they benefit from the formalism, accept it works, but pay a cost in marginalization for pursuing interpretations that threaten the Copenhagen consensus; higher exit option than determinists, higher power). practical_quantum_engineers: d ≈ 0.15 (beneficiary—use the math without enforced ontological commitment, arbitrage option available). Beneficiaries/victims are declared as: measurement_foundationalism_research_program (beneficiary), deterministic_interpretation_advocates and realist_ontology_researchers (victims).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT exhibit mandatrophy. The founding problem (the measurement problem) is still contested and live, not dead. Multiple solutions coexist (Copenhagen, many-worlds, pilot-wave, GRW objective collapse). The constraint persists because it is maintained institutionally—textbook standards, journal gatekeeping, pedagogical canon—not because the problem it was built to solve has disappeared. The founding_problem_status is 'contested', not 'dead', so the mismatch gate (dead + world_rearranges = zombie/mandatrophy flag) does not fire. The theater_ratio rise (0.15 → 0.42) signals that enforcement activity is increasingly performative (presenting one interpretation as neutral, standard, obvious rather than defended), but theatricality is a symptom of extraction difficulty, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collapse_as_physical_fact,
    'Is wavefunction collapse a real physical process, or a calculational tool (epistemic device) for updating beliefs about measurement outcomes?',
    'Empirical detection: if collapse produces any physical signature detectable outside the measurement context (energy release, gravitational effect, particle tracks), it is physical. If no such signature exists and all predictions match decoherence without collapse, then collapse is epistemic.',
    'If collapse is physical, the Copenhagen reading''s core premise stands. If collapse is epistemic, it is a way of talking about measurement, not an ontological fact—the sibling interpretations (many-worlds, pilot-wave) handle the same physics without positing collapse as primitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_as_physical_fact, empirical, 'Whether wavefunction collapse is a physical process or a calculational device.').

omega_variable(
    observer_eliminability,
    'Is the observer (measurement apparatus, conscious agent, external system interacting with the quantum system) a non-eliminable feature of the quantum formalism, or can quantum mechanics be formulated entirely in terms of evolution and decoherence without privileging measurement events?',
    'Formal reconstruction: many-worlds and decoherent-histories approaches have built quantum mechanics without primitive measurement. If they are complete and empirically equivalent, the observer is eliminable; Copenhagen''s necessity claim fails.',
    'If the observer is eliminable, the Copenhagen reading loses a central organizing principle—measurement-as-primitive becomes pedagogical convenience rather than foundational requirement. This opens space for deterministic alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observer_eliminability, conceptual, 'Whether the observer role is structurally necessary or empirically eliminable.').

omega_variable(
    interpretation_contest_framing,
    'Is the quantum interpretation question a genuine empirical dispute waiting for better experiments, or a conceptual/philosophical debate where multiple interpretations are empirically indistinguishable?',
    'If empirical tests distinguish interpretations (e.g., collapse models produce detectable energy loss; pilot-wave produces effects hidden variables forbid), the dispute is empirical. If all viable interpretations remain empirically equivalent after 100 years of experiments, the dispute is conceptual.',
    'If empirical, then institutional suppression of alternative research is scientifically illegitimate—fair competition should be restored. If conceptual, then institutional preference for one interpretation is a matter of theoretical taste, and the suppression problem is deflated (but the constraint is still extraction, just less harmful).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretation_contest_framing, conceptual, 'Whether the interpretation contest is empirical or philosophical in character.').

omega_variable(
    kernel_reading_vs_independent_constraint,
    'Is this constraint best understood as one reading of a deeper kernel (the quantum formalism''s mathematical structure), or as a standalone constraint about Copenhagen''s institutional dominance?',
    'If the constraint''s classification and extraction profile depend on the reading frame (Copenhagen vs. many-worlds vs. pilot-wave), then it is a reading-indexed constraint. If the extraction profile is stable across readings (institutional gatekeeping is extraction regardless of interpretation), then institutional dominance is the constraint, not the reading.',
    'If reading-indexed, the constraint family must be fully decomposed: one story per reading, linked via network.affects_constraints. If standalone, the institutional-dominance constraint could be split from the interpretive-reading constraints, with different metrics for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_independent_constraint, conceptual, 'Whether this is a reading of the quantum formalism kernel or an independent institutional constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1925, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1925, quantum_formalism__copenhagen_reading, theater_ratio, 1925, 0.15).
narrative_ontology:measurement_basis(quan_tr_t1925, observed).
narrative_ontology:measurement(quan_tr_t1950, quantum_formalism__copenhagen_reading, theater_ratio, 1950, 0.22).
narrative_ontology:measurement_basis(quan_tr_t1950, observed).
narrative_ontology:measurement(quan_tr_t1975, quantum_formalism__copenhagen_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement_basis(quan_tr_t1975, observed).
narrative_ontology:measurement(quan_tr_t2000, quantum_formalism__copenhagen_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(quan_tr_t2000, observed).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__copenhagen_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(quan_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(quan_be_t1925, quantum_formalism__copenhagen_reading, base_extractiveness, 1925, 0.45).
narrative_ontology:measurement_basis(quan_be_t1925, observed).
narrative_ontology:measurement(quan_be_t1950, quantum_formalism__copenhagen_reading, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement_basis(quan_be_t1950, observed).
narrative_ontology:measurement(quan_be_t1975, quantum_formalism__copenhagen_reading, base_extractiveness, 1975, 0.64).
narrative_ontology:measurement_basis(quan_be_t1975, observed).
narrative_ontology:measurement(quan_be_t2000, quantum_formalism__copenhagen_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement_basis(quan_be_t2000, observed).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__copenhagen_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(quan_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1925, quantum_formalism__copenhagen_reading, suppression_requirement, 1925, 0.35).
narrative_ontology:measurement_basis(quan_su_t1925, observed).
narrative_ontology:measurement(quan_su_t1950, quantum_formalism__copenhagen_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement_basis(quan_su_t1950, observed).
narrative_ontology:measurement(quan_su_t1975, quantum_formalism__copenhagen_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement_basis(quan_su_t1975, observed).
narrative_ontology:measurement(quan_su_t2000, quantum_formalism__copenhagen_reading, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement_basis(quan_su_t2000, observed).
narrative_ontology:measurement(quan_su_t2025, quantum_formalism__copenhagen_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(quan_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.25).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This constraint is a member of the quantum_formalism constraint family. The kernel is the stabilized mathematical structure of quantum mechanics (Schrödinger equation, Born rule, measurement postulate). The Copenhagen reading interprets the kernel as follows: wavefunction collapse is physical, measurement is primitive, indeterminism is real. Many-worlds reading: collapse is apparent, all branches are real, determinism is universal. Pilot-wave reading: particles have positions, guided by the wavefunction as a physical field, determinism is restored via hidden variables. All three readings are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different omegas. They are linked via network.affects_constraints because Copenhagen's institutional dominance influences the resource availability and legitimacy conditions for the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_formalism__copenhagen_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
