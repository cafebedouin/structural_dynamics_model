% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Explicit Systematic Phonics-First Reading Instruction
 *   domain: educational/psychological/cognitive
 *
 * SUMMARY:
 *   This constraint instantiates the phonics-first reading of the
 *   literacy_acquisition_kernel: the claim that reading acquisition requires
 *   explicit, systematic instruction in phoneme-grapheme correspondence
 *   before exposure to connected text, with decoding preceding and enabling
 *   comprehension. It operates as a tangled ropeâgenuinely coordinating
 *   decoding skill for students with weak phonological awareness while
 *   simultaneously extracting professional autonomy from classroom teachers
 *   through mandated scripts, scope-and-sequence lock-in, and policy
 *   evaluation criteria that disfavor responsive instruction. The kernel
 *   decomposes into four structurally distinct readings (phonics-first,
 *   whole-language, balanced-literacy, structured-literacy), each with
 *   different epsilon values and beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - classroom_teachers: Primary payer (moderate/constrained) â bear extraction through scripted lessons and loss of instructional autonomy
 *   - students_with_weak_phonological_awareness: Primary beneficiary (powerless/trapped) â receive systematic decoding instruction that reduces failure risk
 *   - curriculum_policy_setters: Agenda-setter (institutional/arbitrage) â mandate phonics-first programs and control rulemaking
 *   - whole_language_advocates: Excluded voice (organized/constrained) â structurally marginalized by phonics-dominant policy
 *   - reading_researchers: Observer (analytical/analytical) â provide empirical evidence without experiencing extraction or benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.6).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Explicit Systematic Phonics-First Reading Instruction").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational/psychological/cognitive").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '518fdfa7-52e5-464f-930f-686a083ce34d').
narrative_ontology:cs_kernel_codification('518fdfa7-52e5-464f-930f-686a083ce34d', distributed).
narrative_ontology:cs_authority_grounding('518fdfa7-52e5-464f-930f-686a083ce34d', expertise).
narrative_ontology:cs_interpretation_layer_present('518fdfa7-52e5-464f-930f-686a083ce34d').
narrative_ontology:cs_reading_relation('518fdfa7-52e5-464f-930f-686a083ce34d', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('518fdfa7-52e5-464f-930f-686a083ce34d', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('518fdfa7-52e5-464f-930f-686a083ce34d', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('518fdfa7-52e5-464f-930f-686a083ce34d', foundational, decoding_precedes_comprehension).
narrative_ontology:cs_axiom_status(decoding_precedes_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('518fdfa7-52e5-464f-930f-686a083ce34d', decoding_precedes_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('518fdfa7-52e5-464f-930f-686a083ce34d', foundational, systematic_phonics_before_text_mandatory).
narrative_ontology:cs_axiom_status(systematic_phonics_before_text_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('518fdfa7-52e5-464f-930f-686a083ce34d', systematic_phonics_before_text_mandatory, empirically_contingent).
narrative_ontology:cs_reference_frame('518fdfa7-52e5-464f-930f-686a083ce34d', explicit_decoding_primacy).
narrative_ontology:cs_drift_state('518fdfa7-52e5-464f-930f-686a083ce34d', contemporary_policy_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('518fdfa7-52e5-464f-930f-686a083ce34d', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, phonological_awareness_causality).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, decoding_precedence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deliver reading instruction under mandated phonics-first scope-and-sequence documents and scripted curricula. Their professional judgment in selecting methods, pacing, and text choice is constrained by policy and evaluation rubrics. Exit means leaving the profession or teaching in non-regulated settings.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Receive explicit systematic phonics instruction designed to build decoding skills before exposure to connected text. For this population the constraint reduces the risk of reading failure. They have no choice of instructional method; their benefit is structurally coupled to the constraint's operation.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, local).

% State education agencies and legislative bodies that mandate phonics-first curricula, approve scripted commercial programs, and align teacher evaluation to explicit instruction fidelity. They set the rules and can pivot if political or research consensus shifts.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_policy_setters, agenda_setter,
    institutional, generational, arbitrage, national).

% Researchers and practitioners who argue that reading emerges from meaningful engagement with whole text. Their pedagogical approach is structurally excluded from classrooms and teacher-preparation programs by phonics mandates. They would object to the sequencing constraint but are not in the policy conversation in phonics-dominant jurisdictions.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% Cognitive scientists and educational psychologists who study reading acquisition mechanisms. They produce empirical evidence on phonological awareness, decoding, and comprehension without directly experiencing the constraint's extraction or benefit.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that beginning readers, especially those with weak phonological awareness, receive systematic instruction in sound-letter mapping so that decoding failure is reduced.
% TRANSFER_FUNCTION: Transfers instructional control from classroom teachers to centralized curriculum scripts and scope-and-sequence mandates; transfers decoding skill from the instructional environment to the student.
% ABSENT_VOICES: Whole language advocates and teachers favoring responsive, child-led literacy instruction are excluded from policy design in jurisdictions where phonics mandates dominate.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, classroom teachers would regain autonomy to select and sequence literacy methods; students with weak phonological awareness might face higher failure rates without guaranteed systematic decoding instruction, while whole-language pedagogies would resurface in classrooms and teacher preparation.
% FOUNDING_PROBLEM: High rates of reading failure, particularly among students with weak phonological awareness, attributed to inconsistent or absent systematic decoding instruction.
% FOUNDING_PROBLEM_CORROBORATION: Reading researchers outside the policy advocacy community attest that decoding instruction is necessary but contest whether the phonics-first sequencing constraint is required for all students; some corroborate that the problem is live for dyslexic populations but not universally. Whole-language advocates contest the problem framing entirely.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is moderate-high because the constraint systematically displaces teacher judgment with centralized curricular control; suppression (0.60) reflects active policy enforcement that marginalizes whole-language alternatives. Theater_ratio (0.35) captures rising performative compliance as schools implement scripted programs to satisfy audits rather than student need. Accessibility_collapse (0.45) is incomplete because alternative pedagogies persist in sub rosa practice and homeschooling. Resistance (0.50) acknowledges sustained pushback from whole-language advocates and teacher professionalism movements. The measurement series share one aligned time grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary reform that solves a coordination failure (reading failure); the payer seat experiences it as deprofessionalization and deskilling. The beneficiary seat experiences it as an enabling scaffold. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with weak phonological awareness are declared beneficiaries and sit near the full-beneficiary end (low d, subsidized by the constraint). Classroom teachers are declared victims/payers and sit near the full-target end (high d, amplified extraction). Policy setters administer the constraint and hold arbitrage-grade exit; their derived d is low because they control the rule, but they are not direct beneficiaries of the monetary or status transfer. Whole-language advocates are excluded and would face high d if inside the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâhigh decoding failure among at-risk readersâis contested as to scope and remedy. If the problem is genuinely live only for a subset but the constraint is enforced universally, the mandate has outlived its proportional justification and the classification path tends toward snare. If the problem remains live and the scripting is necessary, it stays tangled rope. The temporal measurements show extraction and theater rising together, suggesting coordination decay into enforcement theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonics_universality_contingent,
    'Is explicit phonics-first instruction universally necessary for all learners, or only for those with weak phonological awareness?',
    'Longitudinal comparative trials measuring decoding and comprehension outcomes for phonics-first versus balanced or whole-language approaches across diverse learner profiles.',
    'If universality fails, the constraint''s extraction from teacher autonomy is justified only for a subset of students, shifting classification toward snare for the general population; if upheld, the coordination function broadens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_universality_contingent, empirical, 'Whether phonics-first sequencing is necessary for all learners or only a subset').

omega_variable(
    teacher_scripting_extraction_necessity,
    'Does the benefit to students with weak phonological awareness require the degree of teacher autonomy loss (scripting) observed in phonics-first mandates?',
    'Comparative policy analysis across jurisdictions with high versus low scripting requirements, controlling for student population and resource levels.',
    'If equivalent outcomes are achieved with less scripting, the measured extraction includes avoidable overhead, sharpening the tangled-rope profile; if scripting is necessary, extraction is inherent to the coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_scripting_extraction_necessity, conceptual, 'Whether teacher autonomy loss is structurally necessary for the student benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__phonics_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__phonics_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the literacy_acquisition_kernel constraint family. The kernel 'reading acquisition' decomposes into structurally distinct claims: phonics-first, whole-language, balanced-literacy, and structured-literacy. Each claim carries a different epsilon, different beneficiary/victim structures, and different classifications. This reading instantiates the phonics-first claim, which asserts that decoding must precede connected text exposure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
