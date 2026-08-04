% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Copenhagen Interpretation: Measurement as Primitive Ontological Boundary
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The Copenhagen reading of quantum mechanics treats wavefunction collapse
 *   as a physical process and measurement as a primitive ontological category
 *   that cannot be reduced to unitary dynamics. This reading functions as a
 *   constraint on physical theorizing: it legitimates the refusal to solve
 *   the measurement problem, channels research away from realist
 *   interpretations, and stabilizes an orthodox consensus through textbook
 *   canon and peer-review norms. It is one reading of the quantum_formalism
 *   kernel, coexisting with many-worlds and pilot-wave siblings that reject
 *   the measurement primitive.
 *
 * KEY AGENTS:
 *   - orthodox_physics_community: Primary agenda-setter (institutional/arbitrage) — enforces interpretive orthodoxy through curriculum and peer review
 *   - working_physicists: Primary beneficiary (organized/constrained) — gains calculational efficiency without foundational burden
 *   - alternative_interpretation_researchers: Primary target (powerless/trapped) — bears exclusion from mainstream funding and positions
 *   - foundational_researchers: Secondary target (moderate/constrained) — faces marginalization of research questions
 *   - quantum_foundations_observers: Analytical observer — sees the full structural asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.62).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.58).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Interpretation: Measurement as Primitive Ontological Boundary").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, '7d1c65f9-96af-410c-87bd-2fe53eec7475').
narrative_ontology:cs_kernel_codification('7d1c65f9-96af-410c-87bd-2fe53eec7475', formalized).
narrative_ontology:cs_authority_grounding('7d1c65f9-96af-410c-87bd-2fe53eec7475', expertise).
narrative_ontology:cs_interpretation_layer_present('7d1c65f9-96af-410c-87bd-2fe53eec7475').
narrative_ontology:cs_reading_relation('7d1c65f9-96af-410c-87bd-2fe53eec7475', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d1c65f9-96af-410c-87bd-2fe53eec7475', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('7d1c65f9-96af-410c-87bd-2fe53eec7475', foundational, measurement_primitive_ontology).
narrative_ontology:cs_axiom_status(measurement_primitive_ontology, holdable).
narrative_ontology:cs_axiom_grounding('7d1c65f9-96af-410c-87bd-2fe53eec7475', measurement_primitive_ontology, conventional).
narrative_ontology:cs_axiom('7d1c65f9-96af-410c-87bd-2fe53eec7475', foundational, irreducible_measurement_indeterminism).
narrative_ontology:cs_axiom_status(irreducible_measurement_indeterminism, holdable).
narrative_ontology:cs_axiom_grounding('7d1c65f9-96af-410c-87bd-2fe53eec7475', irreducible_measurement_indeterminism, conventional).
narrative_ontology:cs_reference_frame('7d1c65f9-96af-410c-87bd-2fe53eec7475', copenhagen_operational_framework).
narrative_ontology:cs_drift_state('7d1c65f9-96af-410c-87bd-2fe53eec7475', post_bell_decoherence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7d1c65f9-96af-410c-87bd-2fe53eec7475', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, orthodox_physics_community).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, working_physicists).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, foundational_researchers).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, wavefunction_collapse_doctrine).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, born_rule_irreducibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls textbook canon, peer-review norms, and hiring standards in physics departments worldwide. Presents the Copenhagen framework as the default interpretive stance. Could adopt alternative interpretive frameworks but maintains the status quo because it stabilizes curriculum, preserves departmental boundaries, and avoids reopening expensive foundational questions.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, orthodox_physics_community, agenda_setter,
    institutional, generational, arbitrage, global).

% Uses the Copenhagen calculational framework for predictions without requiring a solution to the measurement problem. Benefits from a shared language that avoids metaphysical disputes in day-to-day research. Exit is constrained by the need to publish in orthodox journals and collaborate within mainstream paradigms.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, working_physicists, beneficiary,
    organized, biographical, constrained, global).

% Researchers developing Bohmian, Everettian, and objective-collapse interpretations. Face systematic exclusion from mainstream journals, funding panels, and faculty positions when their work challenges the Copenhagen measurement primitive. Their research is frequently dismissed as mere philosophy regardless of technical rigor.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, alternative_interpretation_researchers, payer,
    powerless, generational, trapped, global).

% Physicists and philosophers working on the measurement problem, decoherence, and quantum foundations. Their questions are treated as peripheral to real physics despite being central to ontological clarity. Funding and career advancement are harder to secure than in orthodox application domains.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, foundational_researchers, payer,
    moderate, biographical, constrained, global).

% Philosophers of physics and interdisciplinary scholars who track the interpretive landscape. They observe that the Copenhagen reading persists as institutional default despite the existence of structurally complete alternatives.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_foundations_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, orthodox_physics_community).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified calculational framework and stopping rule for quantum predictions, eliminating the need to resolve the measurement problem before doing physics.
% TRANSFER_FUNCTION: Moves epistemic authority and resource allocation from foundational questions and realist interpretations to orthodox calculational practice, concentrating institutional prestige in mainstream departments.
% ABSENT_VOICES: Bohmian and Everettian researchers are structurally underrepresented in textbook authorship and foundational funding panels; philosophers of physics note that the measurement problem remains unsolved but are treated as peripheral to physical practice.
% DISAPPEARANCE_RATIONALE: If the Copenhagen interpretive boundary vanished, the orthodox consensus that no more can be said about measurement would collapse; research funding and curriculum space would reallocate toward realist interpretations and measurement-problem solutions; textbook canon would require rewriting.
% FOUNDING_PROBLEM: The quantum formalism produced predictions without an agreed ontology; the measurement problem threatened to make the theory incomplete or incoherent; Copenhagen provided a principled refusal to answer unanswerable questions so that physics could proceed.
% FOUNDING_PROBLEM_CORROBORATION: Foundational researchers and philosophers of physics attest that the measurement problem is a live research frontier; the founding justification that the questions are meaningless is contested by those same outside seats and by the existence of viable realist interpretations.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
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
 *   Extractiveness (0.62) is substantial because the interpretation suppresses entire research programs by treating their core questions as meaningless. Suppression (0.58) reflects active institutional barriers in hiring and refereeing. Theater ratio (0.40) captures the growing gap between the performed narrative of physical collapse and the actual heuristic practice of working physicists. Accessibility collapse (0.72) is high because once inside the Copenhagen framework alternatives appear metaphysically extravagant. Resistance (0.55) is moderate, coming from persistent alternative programs and renewed foundational interest. The founding_problem_status is dead and disappearance_verdict is world_rearranges, flagging a zombie tangled rope.
 *
 * PERSPECTIVAL GAP:
 *   The orthodox physics community and working physicists experience the constraint as genuine coordination: it settles disputes, enables calculation, and preserves professional identity. Alternative and foundational researchers experience the same structure as extraction: their questions are ruled out of bounds, their funding is harder to secure, and their ontology is dismissed. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The orthodox_physics_community and working_physicists are declared beneficiaries with constrained or arbitrage exit, placing them toward the beneficiary end (low d, damped effective extraction). Alternative_interpretation_researchers and foundational_researchers are declared victims with trapped or constrained exit, placing them toward the target end (high d, amplified effective extraction). The quantum_foundations_observer seat is analytical with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interpretive paralysis threatening the new quantum theory — was solved by Copenhagen's strategic refusal to interpret. Today, viable realist alternatives (Everett, Bohm, GRW) have dissolved that paralysis, yet the arrangement persists. The R5 genealogy (founding_problem_status: dead + disappearance_verdict: world_rearranges) prevents mislabeling this as a still-functional scaffold or rope. Its persistence beyond the death of its founding problem is exactly what makes it a tangled rope rather than a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copenhagen_naturalness_ambiguity,
    'Does the measurement boundary reflect an irreducible feature of physical reality, or is it a convention stabilizing a particular research tradition?',
    'Experimental detection of objective collapse limits or decisive discrimination between interpretations would resolve naturalness; sociological study of pedagogy would resolve convention.',
    'If purely conventional, extraction is higher and the constraint trends toward snare; if natural, it would reclassify toward mountain or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copenhagen_naturalness_ambiguity, conceptual, 'Whether the measurement primitive is natural law or institutional convention').

omega_variable(
    marginalization_mechanism,
    'Is the persistence of Copenhagen dominance maintained by active institutional suppression of alternatives, or by the genuine instrumental superiority of the calculational framework?',
    'Quantitative analysis of funding allocation, citation networks, and hiring patterns in physics departments relative to predictive success rates across interpretations.',
    'If suppression is structural, classification shifts toward snare; if instrumental superiority explains adoption, extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalization_mechanism, empirical, 'Structural suppression versus instrumental adoption as driver of orthodoxy').

omega_variable(
    foundational_viability,
    'Have viable realist interpretations dissolved the founding problem, or does Copenhagen still coordinate indispensable heuristic practice?',
    'Assessment of whether realist programs achieve equivalent or superior pedagogical and calculational efficacy without the measurement primitive.',
    'If realist programs are viable complete replacements, founding_problem_status=dead is confirmed and the constraint is a zombie tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_viability, empirical, 'Whether alternatives have fully superseded the founding justification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copenhagen_reading_tr_t0, quantum_formalism__copenhagen_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(copenhagen_reading_tr_t20, quantum_formalism__copenhagen_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(copenhagen_reading_tr_t40, quantum_formalism__copenhagen_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(copenhagen_reading_tr_t60, quantum_formalism__copenhagen_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(copenhagen_reading_tr_t80, quantum_formalism__copenhagen_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(copenhagen_reading_tr_t100, quantum_formalism__copenhagen_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(copenhagen_reading_be_t0, quantum_formalism__copenhagen_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(copenhagen_reading_be_t20, quantum_formalism__copenhagen_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(copenhagen_reading_be_t40, quantum_formalism__copenhagen_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(copenhagen_reading_be_t60, quantum_formalism__copenhagen_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(copenhagen_reading_be_t80, quantum_formalism__copenhagen_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(copenhagen_reading_be_t100, quantum_formalism__copenhagen_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(copenhagen_reading_su_t0, quantum_formalism__copenhagen_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(copenhagen_reading_su_t20, quantum_formalism__copenhagen_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(copenhagen_reading_su_t40, quantum_formalism__copenhagen_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(copenhagen_reading_su_t60, quantum_formalism__copenhagen_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(copenhagen_reading_su_t80, quantum_formalism__copenhagen_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(copenhagen_reading_su_t100, quantum_formalism__copenhagen_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% The quantum_formalism kernel supports multiple interpretation readings. This copenhagen_reading instantiates the orthodox operationalist reading; siblings instantiate deterministic and realist readings. They share the same formal kernel but diverge on measurement ontology and the status of the wavefunction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
