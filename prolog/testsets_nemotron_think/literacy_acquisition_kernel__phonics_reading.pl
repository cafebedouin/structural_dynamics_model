% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Explicit Systematic Phonics-First Reading Acquisition Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   The phonics_reading instantiates the claim that reading acquisition
 *   requires explicit, systematic instruction in phoneme-grapheme
 *   correspondence before connected text exposure — decoding precedes and
 *   enables comprehension. This reading has become the basis for statewide
 *   'science of reading' mandates requiring scripted phonics programs,
 *   fidelity monitoring, and teacher retraining. The constraint presents as a
 *   coordination mechanism (solving the reading failure problem) but operates
 *   with high extraction on teacher professional autonomy through scripted
 *   lessons, mandated pacing, and compliance monitoring. Beneficiaries are
 *   students with weak phonological awareness who reliably fail without
 *   systematic decoding instruction. Victims are classroom teachers whose
 *   diagnostic judgment is displaced by program fidelity. The claimed_type is
 *   tangled_rope: genuine coordination function (reducing decoding failure)
 *   combined with asymmetric extraction (teacher autonomy) requiring active
 *   enforcement (state mandates, curriculum adoption lists, literacy
 *   coaches).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.45).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Explicit Systematic Phonics-First Reading Acquisition Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '4d4844a4-57bc-474a-902f-7df96c9244b8').
narrative_ontology:cs_kernel_codification('4d4844a4-57bc-474a-902f-7df96c9244b8', formalized).
narrative_ontology:cs_authority_grounding('4d4844a4-57bc-474a-902f-7df96c9244b8', expertise).
narrative_ontology:cs_interpretation_layer_present('4d4844a4-57bc-474a-902f-7df96c9244b8').
narrative_ontology:cs_reading_relation('4d4844a4-57bc-474a-902f-7df96c9244b8', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('4d4844a4-57bc-474a-902f-7df96c9244b8', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d4844a4-57bc-474a-902f-7df96c9244b8', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('4d4844a4-57bc-474a-902f-7df96c9244b8', foundational, explicit_systematic_phonics_prerequisite).
narrative_ontology:cs_axiom_status(explicit_systematic_phonics_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('4d4844a4-57bc-474a-902f-7df96c9244b8', explicit_systematic_phonics_prerequisite, empirically_contingent).
narrative_ontology:cs_axiom('4d4844a4-57bc-474a-902f-7df96c9244b8', secondary, decoding_enables_comprehension).
narrative_ontology:cs_axiom_status(decoding_enables_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('4d4844a4-57bc-474a-902f-7df96c9244b8', decoding_enables_comprehension, empirically_contingent).
narrative_ontology:cs_reference_frame('4d4844a4-57bc-474a-902f-7df96c9244b8', phonics_first_instructional_sequence).
narrative_ontology:cs_drift_state('4d4844a4-57bc-474a-902f-7df96c9244b8', contemporary_reading_science_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4d4844a4-57bc-474a-902f-7df96c9244b8', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, phonemic_awareness_predicts_reading_success).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, explicit_decoding_instruction_prevents_reading_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children who lack strong phonological processing skills; without explicit systematic phonics instruction they experience high rates of reading failure. They have no choice in instructional method and cannot exit the school system. The constraint delivers them a decoding foundation they would not reliably acquire through exposure alone.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, local).

% Lose professional autonomy to scripted phonics programs with mandated pacing, scripted teacher talk, and limited instructional discretion. Must implement curriculum fidelity measures rather than exercise diagnostic judgment. Exit options are constrained: can change districts or leave profession, but mandates are widespread. Bear the extraction of professional judgment while being held accountable for outcomes.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers, payer,
    organized, biographical, constrained, local).

% Produce and sell scripted phonics programs (e.g., LETRS-aligned, Orton-Gillingham commercial variants) mandated by state adoption lists. Capture revenue from district purchases and recurring professional development. Can arbitrage across state markets; the constraint creates their market. Their interest aligns with mandate expansion.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers, beneficiary,
    institutional, biographical, arbitrage, national).

% Cognitive scientists and reading researchers (e.g., Dehaene, Seidenberg, Castles) who study reading acquisition neural mechanisms. Provide the evidence base cited for phonics-first policies. Do not collect rents from the constraint but their work legitimates it. Can exit the discourse freely; their professional standing depends on empirical rigor, not policy alignment.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, literacy_researchers, observer,
    analytical, generational, analytical, universal).

% Teachers and scholars advocating meaning-centered, exposure-based reading instruction. Structurally excluded from current policy tables, curriculum adoption committees, and major professional development funding. Would object to scripted phonics mandates as harmful to motivation and comprehension. Their exit is constrained: marginalized in hiring, publishing, and conference circuits.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_practitioners, excluded,
    organized, generational, constrained, national).

% State legislators, boards of education, and chiefs who mandate phonics-first curricula, require specific program adoption, and tie funding to compliance. Set the enforcement machinery (literacy coaches, fidelity walkthroughs, retention laws). Do not directly extract but control the constraint's scope and enforcement intensity.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, education_policymakers, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures all students, especially those with weak phonological awareness, acquire reliable decoding skills through explicit systematic instruction before facing connected text demands, preventing the reading failure cascade that occurs when decoding is left to incidental discovery.
% TRANSFER_FUNCTION: Moves instructional authority from teachers' professional judgment (diagnostic, responsive, flexible) to scripted curriculum programs (standardized, paced, fidelity-measured); moves student outcome variance from high (constructivist exposure) to lower (systematic skill-building); moves financial resources from district discretion to publisher contracts.
% ABSENT_VOICES: Whole language and balanced literacy practitioners who would argue that motivation, comprehension, and reading identity develop through meaningful text engagement, not isolated decoding drills; students who might thrive with different instructional sequences or multimodal literacy approaches; parents in communities where scripted programs clash with cultural literacy practices.
% DISAPPEARANCE_RATIONALE: If phonics-first mandates and scripted program requirements vanished overnight, curriculum adoption would revert to local control, teacher preparation would shift from program fidelity to diagnostic pedagogy, publisher revenue streams would collapse, and the instructional landscape would fragment into varied approaches — some systematic, some constructivist, some mixed. Student outcomes would diverge more widely.
% FOUNDING_PROBLEM: Persistently high rates of reading failure (30-40% below basic on NAEP) under whole language and early balanced literacy approaches, disproportionately affecting students with weak phonological awareness, dyslexia, and limited home literacy environments.
% FOUNDING_PROBLEM_CORROBORATION: National Reading Panel (2000) — independent congressionally mandated review; cognitive neuroscience of reading (Dehaene 2009, Seidenberg 2017) — basic science outside education policy; state-level policy analyses (Mississippi, Colorado, Tennessee) showing gains after phonics mandates — governmental sources not funded by publishers. No single source is uncontested, but convergence across independent domains is notable.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).
:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.45) reflects the constraint's dual nature: low extraction on student outcomes (systematic instruction reduces failure) but high extraction on teacher autonomy (scripted programs). Suppression (0.75) is high because alternatives (whole language, teacher-designed phonics) are actively excluded from adoption lists and professional development. Theater ratio (0.35) captures performative compliance — fidelity walkthroughs, scripted language adherence — that exceeds functional need. Accessibility collapse (0.65) reflects policy closure: few districts can adopt non-mandated approaches. Resistance (0.70) is high from teachers' unions, whole language advocates, and some researchers who contest the mandate's scope. Measurements show extraction and suppression rising over 30 years as mandates expanded from voluntary to universal.
 *
 * PERSPECTIVAL GAP:
 *   From the policymaker/researcher seat, the constraint is a rope: a coordination mechanism solving a collective action problem (reading failure) with minimal coercion (evidence-based). From the teacher seat, it is a snare: extraction of professional autonomy disguised as science, enforced by compliance machinery. From the student seat, it is a scaffold: temporary support for decoding that should fade. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with weak phonological awareness are full beneficiaries (d ≈ 0.0): the constraint subsidizes their decoding acquisition. Classroom teachers are full targets (d ≈ 1.0): they bear the extraction of professional judgment with constrained exit. Curriculum publishers are beneficiaries (d ≈ 0.15) capturing rents but not the primary extraction target. Literacy researchers are analytical observers (d = 0.5). Whole language practitioners are excluded — their exclusion is the enforcement object. Policymakers are agenda setters with analytical exit — they administer but do not personally extract.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reading failure under whole language) is contested as still live. Phonics advocates cite persistent NAEP scores; balanced literacy advocates cite misimplementation. The constraint shows mandatrophy signals: theater rising, suppression hardening, but the coordination function (decoding gains) persists. If the founding problem is dead (reading failure solved), the mandate persists as piton. If live, it remains tangled_rope. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags this for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (systematic decoding instruction reducing failure) end and the extractive component (scripted lessons, mandated pacing, fidelity policing) begin?',
    'Natural experiment comparing student outcomes under scripted-program mandates vs. teacher-designed systematic phonics with equivalent scope/sequence but professional discretion. If outcomes are equivalent, the scripted component is pure extraction.',
    'If scripted mandates add no outcome value over teacher-designed systematic instruction, the constraint''s extraction is entirely overhead — reclassification toward snare for teacher seat. If scripted mandates produce better fidelity/outcomes, part of measured extraction is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether scripted program mandates are necessary for the coordination function or extractive overhead.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does the phonics_reading represent a distinct constraint from structured_literacy_reading, or is it a proper subset that should not be separately classified?',
    'Trace policy adoption: if states mandating ''phonics'' actually adopt structured_literacy programs (LETRS, OG), the readings are empirically fused. If some jurisdictions mandate narrow phonics-first without broader language components, they are distinct.',
    'If fused, ε-invariance requires one constraint story; if distinct, each gets its own ε and classification. Affects network.affects_constraints and cs_structure.reading_relations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether phonics_reading and structured_literacy_reading are empirically separable constraints.').

omega_variable(
    teacher_autonomy_extraction_measurement,
    'How much of the measured suppression (0.75) reflects structural barriers (mandates, adoption lists) versus internalized compliance (teachers self-censoring, performative fidelity)?',
    'Post-mandate teacher surveys measuring perceived autonomy, coupled with observation of actual instructional variation within ''fidelity'' classrooms. If suppression persists after mandate removal, internalized component is significant.',
    'If suppression is largely internalized, the constraint''s effective extraction on teachers is higher than structural measures suggest — teachers carry the constraint after policy exit. Would amplify χ for teacher seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_extraction_measurement, empirical, 'Structural vs. internalized suppression mechanism on teacher seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_tr_t6, literacy_acquisition_kernel__phonics_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_tr_t12, literacy_acquisition_kernel__phonics_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_tr_t18, literacy_acquisition_kernel__phonics_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_tr_t24, literacy_acquisition_kernel__phonics_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_tr_t30, literacy_acquisition_kernel__phonics_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_be_t6, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_be_t12, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_be_t18, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_be_t24, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_be_t30, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_su_t6, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_su_t12, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_su_t18, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_su_t24, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(literacy_acquisition_kernel__phonics_reading_su_t30, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__phonics_reading, 0.08).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint (phonics_reading) and its three siblings decompose the colloquial label 'how children learn to read' into structurally distinct claims with different ε, beneficiaries, and enforcement structures. The phonics_reading claims explicit systematic phonics as prerequisite; whole_language_reading claims it is unnecessary; balanced_literacy_reading claims complementarity; structured_literacy_reading claims broader systematic scope. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__phonics_reading, institutional, 0.15).
constraint_indexing:directionality_override(literacy_acquisition_kernel__phonics_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
