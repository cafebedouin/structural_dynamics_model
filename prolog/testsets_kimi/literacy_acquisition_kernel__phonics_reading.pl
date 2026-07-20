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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Explicit Systematic Phonics-First Reading Acquisition Mandate
 *   domain: educational/psychological/pedagogical
 *
 * SUMMARY:
 *   This constraint is the phonics-first reading of the
 *   literacy_acquisition_kernel. It asserts that reading acquisition requires
 *   explicit, systematic phoneme-grapheme instruction before any connected
 *   text exposure, and that decoding causally precedes comprehension.
 *   Operating as a curriculum mandate in many jurisdictions, it functions as
 *   a tangled rope: it coordinates genuine reading success for students with
 *   weak phonological awareness while simultaneously extracting professional
 *   autonomy from teachers through scripted lessons and fidelity enforcement.
 *   The claim/metric independence is maintained: the constraint is claimed as
 *   a scientifically grounded coordination mechanism, while the authored
 *   metrics describe substantial extraction and active suppression of
 *   alternative pedagogies.
 *
 * KEY AGENTS:
 *   - students_with_weak_phonological_awareness: Primary beneficiary (powerless/constrained) â receives systematic decoding instruction structured to their developmental needs
 *   - classroom_teachers: Primary target/victim (moderate/identity_locked) â loses professional autonomy to scripted curricula and pacing mandates
 *   - curriculum_mandate_authority: Agenda setter (institutional/mobile) â enforces sequencing compliance and controls legitimate instructional scope
 *   - whole_language_advocates: Excluded party (organized/trapped) â displaced from curriculum decisions and policy conversation
 *   - reading_researchers: Analytical observer (analytical/analytical) â evaluates efficacy across populations without direct policy stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.65).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Explicit Systematic Phonics-First Reading Acquisition Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational/psychological/pedagogical").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, 'd48b0ef6-3d11-4ccc-bcfa-22af3f9b6850').
narrative_ontology:cs_kernel_codification('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', fixed_text).
narrative_ontology:cs_authority_grounding('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', expertise).
narrative_ontology:cs_interpretation_layer_present('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850').
narrative_ontology:cs_reading_relation('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', foundational, explicit_phonics_precedes_connected_text).
narrative_ontology:cs_axiom_status(explicit_phonics_precedes_connected_text, holdable).
narrative_ontology:cs_axiom_grounding('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', explicit_phonics_precedes_connected_text, empirically_contingent).
narrative_ontology:cs_axiom('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', foundational, decoding_prerequisite_for_comprehension).
narrative_ontology:cs_axiom_status(decoding_prerequisite_for_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', decoding_prerequisite_for_comprehension, empirically_contingent).
narrative_ontology:cs_reference_frame('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', explicit_systematic_decoding_primacy).
narrative_ontology:cs_drift_state('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', contemporary_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d48b0ef6-3d11-4ccc-bcfa-22af3f9b6850', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive mandated explicit, systematic phonics instruction that prioritizes decoding before exposure to connected text. For this group the constraint structures instruction around their developmental need for phoneme-grapheme mapping. They cannot opt out of the mandated instructional sequence or elect an alternative approach.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, constrained, national).

% Must follow scripted phonics curricula, pacing guides, and scope-and-sequence mandates that override professional judgment about individual student readiness or instructional approach. Their autonomy to select materials, improvise lessons, or integrate comprehension activities prior to decoding mastery is curtailed. Exit is constrained by employment requirements and by professional identity fused to pedagogical agency.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers, payer,
    moderate, biographical, identity_locked, national).

% Sets the requirement that decoding instruction precede connected text exposure, approves and funds scripted curricula, and monitors compliance through benchmark assessments and classroom observation protocols. Controls the legitimate instructional sequence and the criteria for fidelity.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_mandate_authority, agenda_setter,
    institutional, generational, mobile, national).

% Advance a competing theory that reading emerges from meaningful text engagement rather than from explicit phonics-first sequencing. Their programs, materials, and teacher-training pathways are displaced by the mandate; they are structurally absent from the policy conversations that set the constraint's parameters.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_advocates, excluded,
    organized, generational, trapped, national).

% Study the relative efficacy of explicit phonics sequencing versus whole-language and balanced approaches. They evaluate decoding and comprehension outcomes across populations, operating outside the beneficiary and payer seats and without direct stake in the policy mandate.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents reading failure in students who do not spontaneously infer phoneme-grapheme correspondences by ensuring they receive explicit, systematic decoding instruction before encountering text that outstrips their skill.
% TRANSFER_FUNCTION: Moves instructional control from classroom teachers to centralized curriculum scripts and pacing guides; transfers the pedagogical decision about when and how to introduce connected text from teacher judgment to mandated scope-and-sequence authorities.
% ABSENT_VOICES: Whole-language advocates and many experienced classroom teachers who favor emergent literacy or balanced approaches are structurally excluded from curriculum adoption decisions; their materials and training are displaced by the mandate even where they have documented efficacy for some populations.
% DISAPPEARANCE_RATIONALE: If the mandate vanished, teachers would immediately regain discretion to integrate connected text and comprehension activities earlier in the sequence; curriculum publishers would shift product mixes; assessment benchmarks would change; and the population of students with weak phonological awareness would experience more variable instruction, some receiving systematic decoding support and some not.
% FOUNDING_PROBLEM: High rates of reading failure among students who do not acquire decoding skills through incidental text exposure, particularly in the absence of explicit phoneme-grapheme instruction.
% FOUNDING_PROBLEM_CORROBORATION: Independent longitudinal reading assessments and special-education diagnosticians outside the phonics curriculum publishing industry attest that a subset of students persistently fail to acquire decoding without explicit instruction; whole-language researchers and sociolinguists contest that the measured failure rate is an artifact of socioeconomic sorting and inappropriate assessment rather than of instructional method.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.65) is substantial because the mandate decouples instructional control from teacher judgment and embeds it in centralized scripts; suppression (0.75) is higher because the constraint's persistence depends on actively excluding whole-language alternatives and monitoring teacher fidelity. Theater ratio (0.35) reflects moderate performative maintenance: the phonics function is real, but a growing share of enforcement activity defends script compliance rather than student decoding outcomes. Accessibility collapse (0.60) captures the contraction of whole-language and balanced alternatives in mandated jurisdictions. Resistance (0.55) reflects the sustained Reading Wars pushback from teachers and whole-language advocates.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (classroom_teachers) and the beneficiary seat (students_with_weak_phonological_awareness) should compute differently: from the teacher's position the constraint is an imposed extraction of professional judgment enforced by observation and assessment regimes; from the weak phonological awareness student's position the same structure appears as targeted coordination that prevents reading failure. The agenda-setter seat (curriculum authority) experiences a third variant, seeing the arrangement as necessary scientific implementation. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Students_with_weak_phonological_awareness are declared beneficiaries with constrained exit and low power, placing their directionality near the full-beneficiary end (low d, damped extraction). Classroom_teachers are declared victims with identity_locked exit and moderate power, placing their directionality near the full-target end (high d, amplified extraction). The curriculum_mandate_authority is the agenda setter with mobile exit, sitting near the beneficiary end because it collects the extracted autonomy as institutional control. Whole_language_advocates are excluded rather than coordinated; their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a rope because the coordination function is inseparable from asymmetric extraction: the same scripted sequencing that supports weak decoders simultaneously strips teacher autonomy. It is not a snare because the coordination function is genuine and empirically supported for a specific population; reading failure is not a fabricated problem. It is not a scaffold because it carries no sunset clause and is treated as permanent scientific truth rather than a transitional support. Piton is avoided because the active enforcement and beneficiary structure are robust, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonics_reading_kernel_position,
    'Does the empirical evidence for explicit phonics instruction support the sequencing constraint (decoding before connected text) as a natural law of reading acquisition, or as one contested pedagogical reading among several?',
    'Meta-analytic review separating decoding outcomes from comprehension and motivation outcomes, compared across jurisdictions where alternative sequencing is permitted.',
    'If the constraint is a natural law it warrants Mountain classification; if it is one reading among several with differential distributional consequences, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_reading_kernel_position, conceptual, 'Whether phonics-first sequencing is a natural law or a contested reading').

omega_variable(
    teacher_suppression_mechanism,
    'Is the loss of teacher autonomy a structural enforcement of scripted curricula, or an internalized belief among teachers that phonics instruction is professionally illegitimate?',
    'Survey of teacher practice in districts where phonics mandates were lifted: do teachers revert to discretionary practice or maintain scripted pacing?',
    'If internalized, effective suppression is higher than structural measures suggest and the constraint operates partly through professional identity lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_suppression_mechanism, empirical, 'Structural versus internalized suppression of teacher autonomy').

omega_variable(
    scripted_lesson_extraction_boundary,
    'To what extent does the systematic phonics mandate require scripting that extracts from teachers, versus permitting explicit phonics within teacher-directed professional judgment?',
    'Comparative analysis of teacher autonomy and student outcomes under scripted versus explicit-but-flexible phonics programs.',
    'If explicit phonics can be delivered without scripting, the extraction on teacher autonomy is separable from the coordination function; if scripting is structurally required by the phonics-first logic, the constraint is irreducibly tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scripted_lesson_extraction_boundary, conceptual, 'Separability of coordination and extraction components').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__phonics_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__phonics_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__phonics_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(lite_tr_t32, literacy_acquisition_kernel__phonics_reading, theater_ratio, 32, 0.34).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__phonics_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(lite_be_t32, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(lite_su_t32, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'literacy acquisition' conflates multiple structurally distinct pedagogical claims. This story isolates the phonics-first sequencing claim; sibling stories isolate whole-language, balanced-literacy, and structured-literacy readings. Each carries its own epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
