% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Phoneme-Grapheme Decoding Priority (Phonics Reading Kernel)
 *   domain: educational_psychology/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint instantiates the phonics-reading kernel: the claim that
 *   phoneme-grapheme decoding must be taught explicitly and systematically
 *   BEFORE students encounter connected text, and that decoding skill
 *   precedes and enables comprehension. This is ONE reading of the contested
 *   literacy-acquisition kernel. Sibling readings include whole-language
 *   (phonological skills emerge from meaningful text), balanced-literacy
 *   (phonics and connected-text engagement are complementary), and
 *   structured-literacy (decoding is one of five essential components,
 *   designed for dyslexia but universal). The phonics-reading frame has been
 *   enacted into state standards and curriculum mandates in most US
 *   jurisdictions, making it structurally powerful. However, the constraint's
 *   persistence depends on actively suppressing alternative instructional
 *   frameworks (whole-language, balanced-literacy) and constraining teacher
 *   professional judgment. The constraint benefits students with weak
 *   phonological awareness (the founding target) by providing systematic
 *   scaffolding they would not acquire incidentally. It extracts from
 *   teachers (autonomy over pacing and text selection) and from students in
 *   low-print environments (who acquire phonological awareness naturally and
 *   experience the constraint as motivation-damping drill). This is
 *   tangled-rope: genuine coordination function (systematic decoding
 *   instruction reduces failure for at-risk students) coupled with asymmetric
 *   extraction (teacher autonomy, student choice in text engagement).
 *
 * KEY AGENTS:
 *   - Classroom teachers: payer seat; constrained exit; bear the labor cost of scripted phoneme-grapheme sequences and loss of instructional judgment
 *   - Students with phonological weakness: beneficiary seat; trapped exit; receive systematic scaffolding that prevents decoding failure
 *   - Emergent readers in low-print environments: victim seat; trapped exit; experience phoneme-grapheme drills as decontextualized before meaningful text, reducing motivation
 *   - Reading researchers: agenda-setter seat; arbitrage exit; conducted meta-analyses establishing phoneme-grapheme instruction, shaped standards, benefit from validation
 *   - Curriculum publishers: beneficiary seat; arbitrage exit; profit from market for scripted phonics curricula
 *   - Balanced-literacy and whole-language advocates: excluded seat; constrained exit; objections treated as ideological, not seated in policy authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.72).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phoneme-Grapheme Decoding Priority (Phonics Reading Kernel)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '3b4a033a-a6db-4089-8160-cb584d0f3d0e').
narrative_ontology:cs_kernel_codification('3b4a033a-a6db-4089-8160-cb584d0f3d0e', fixed_text).
narrative_ontology:cs_authority_grounding('3b4a033a-a6db-4089-8160-cb584d0f3d0e', expertise).
narrative_ontology:cs_interpretation_layer_present('3b4a033a-a6db-4089-8160-cb584d0f3d0e').
narrative_ontology:cs_reading_relation('3b4a033a-a6db-4089-8160-cb584d0f3d0e', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('3b4a033a-a6db-4089-8160-cb584d0f3d0e', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('3b4a033a-a6db-4089-8160-cb584d0f3d0e', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('3b4a033a-a6db-4089-8160-cb584d0f3d0e', foundational, decoding_precedes_comprehension).
narrative_ontology:cs_axiom_status(decoding_precedes_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('3b4a033a-a6db-4089-8160-cb584d0f3d0e', decoding_precedes_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('3b4a033a-a6db-4089-8160-cb584d0f3d0e', foundational, explicit_phonemic_instruction_necessary).
narrative_ontology:cs_axiom_status(explicit_phonemic_instruction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3b4a033a-a6db-4089-8160-cb584d0f3d0e', explicit_phonemic_instruction_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('3b4a033a-a6db-4089-8160-cb584d0f3d0e', phoneme_grapheme_primacy).
narrative_ontology:cs_drift_state('3b4a033a-a6db-4089-8160-cb584d0f3d0e', contemporary_pedagogical_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3b4a033a-a6db-4089-8160-cb584d0f3d0e', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_phonological_weakness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, struggling_early_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, emergent_readers_in_low_print_environments).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).

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
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint constrains teacher discretion on pacing and text selection, and because curriculum publishers profit from the scripted-phonics market. Suppression is higher (0.72) because the constraint's persistence requires active enforcement through state standards, textbook adoption processes, and teacher training programs that discourage alternative frameworks. Theater ratio is moderate (0.41): the phoneme-grapheme sequencing has a genuine pedagogical rationale (systematic instruction does improve decoding outcomes for at-risk students), but an increasing share of the constraint's enforcement infrastructure defends the priority against whole-language and balanced-literacy challenges, which is partly ideological theater rather than functional necessity. Accessibility collapse is moderate (0.65): alternatives (whole-language, balanced-literacy, teacher-directed pacing) exist and have empirical defenders, but the enacted standards make them harder to access in practice. Resistance is substantial (0.58): many educators and parents resist phoneme-grapheme-first sequencing as demotivating for strong readers and as reducing instructional flexibility. The measurement series track extractiveness rising over time as curriculum mandates harden and the constraint becomes increasingly institutionalized, and suppression rising as state standards and teacher training become more tightly aligned to the phoneme-grapheme priority.
 *
 * PERSPECTIVAL GAP:
 *   From the reading-researcher and curriculum-publisher seats, the constraint appears as a successful translation of evidence into practice—a coordination mechanism that solved the early-reading-failure problem. From the classroom-teacher seat, the same constraint appears as loss of professional judgment and scripted delivery. From the whole-language-advocate seat, the constraint appears as enforcement of one research paradigm over another, despite competing empirical evidence on student engagement. From the student-motivation perspective, the constraint's early impact (0.28 theater ratio at t=0) was genuine functionality (students with weak phonological awareness do benefit), but over time (theater ratio rises to 0.41 by t=25) an increasing share of enforcement effort has gone into suppressing alternative frameworks in policy and teacher training, which is partly theater (ideological defense) rather than functional coordination. The engine should compute a divergence here: the reading-researcher and curriculum-publisher seats compute toward rope (genuine coordination for a target population); the teacher and whole-language-advocate seats compute toward snare (enforced choice with suppressed alternatives); the student-motivation seat computes toward extracted labor (motivation cost to non-target students).
 *
 * DIRECTIONALITY LOGIC:
 *   Teachers occupy the payer seat (d near 1.0, full target): they lose autonomy and must implement scripted sequences. Students with phonological weakness occupy the beneficiary seat (d near 0.0, full beneficiary): they receive systematic scaffolding that prevents decoding failure, which is precisely the function the constraint exists to deliver. Emergent readers in low-print environments occupy a victim seat (d elevated from baseline due to constrained exit and demotivation from decontextualized drill). Curriculum publishers and reading researchers benefit but do not administer the constraint directly; they are co-beneficiaries with moderate positive directionality. The constraint's structural asymmetry is stable: the coordination benefit (systematic decoding instruction) concentrates on students with phonological weakness, while the extraction (teacher autonomy loss) disperses across all teachers regardless of their students' baseline phonological awareness. This is the signature structure of tangled rope: one group (at-risk students) is genuinely coordinated and protected; another group (teachers) pays through autonomy loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT show mandatrophy: the founding problem remains live and contested (early reading failure still occurs for students with weak phonological awareness; the question is whether phoneme-grapheme-first sequencing is the necessary solution or whether balanced approaches produce comparable outcomes). The constraint is actively maintained and enforced, not atrophied through inertia. However, the constraint does show signs of extraction creep: it began as a targeted intervention for struggling readers and has been universalized into a one-size-fits-all approach, which extracts motivation costs from students who acquire phonological awareness incidentally. This is NOT mandatrophy (the founding function is still served for the target population); it is drift from targeted to universal application without corresponding adjustment to implementation. The theater-ratio rise from 0.28 to 0.41 reflects this: the functional (targeted) component shrinks as a proportion of total enforcement effort, while ideological suppression of alternative frameworks rises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonological_awareness_prerequisite,
    'Is explicit, systematic phoneme-grapheme instruction a necessary PREREQUISITE for students to acquire decoding skill, or is it one effective PATHWAY among others?',
    'Longitudinal comparison of students taught via phoneme-grapheme-first vs. balanced (simultaneous phonics + connected-text) vs. whole-language pathways, controlling for baseline phonological awareness and home literacy environment. Outcome measures: decoding automaticity, comprehension, intrinsic reading motivation.',
    'If phoneme-grapheme instruction is necessary (only pathway), the constraint''s universalization is justified and extraction is coordination cost. If phoneme-grapheme instruction is one pathway (one among several effective methods), the constraint''s universal enforcement is unnecessary extraction, and alternative frameworks should not be suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonological_awareness_prerequisite, empirical, 'Whether explicit phoneme-grapheme instruction is a necessary prerequisite or one of several effective pathways for decoding acquisition.').

omega_variable(
    causality_vs_correlation_decoding_comprehension,
    'Does decoding skill CAUSE comprehension, or do both decoding and comprehension emerge from a shared underlying phonological awareness base?',
    'Intervention studies manipulating decoding fluency while controlling for vocabulary, comprehension strategy, and phonological awareness; measurement of comprehension gains and cost-benefit ratio for decoding automation vs. meaning-focused instruction.',
    'If decoding causes comprehension, the phoneme-grapheme-first sequencing is well-founded: build decoding automaticity first, then layer comprehension. If comprehension develops in parallel with decoding (shared foundation), the sequencing priority is weaker, and balanced or meaning-first approaches may be equally effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_vs_correlation_decoding_comprehension, empirical, 'Whether decoding skill causally enables comprehension or whether both emerge from shared phonological processes.').

omega_variable(
    extraction_from_teacher_autonomy,
    'Is the suppression of teacher professional judgment (via scripted phoneme-grapheme sequences) necessary to ensure systematic instruction, or is it an extraction that degrades teacher effectiveness and motivation?',
    'Comparison of student outcomes and teacher retention in schools with scripted phonics vs. schools with teacher-guided phonics within the same phoneme-grapheme framework. Measurement: decoding outcomes, comprehension, student engagement, teacher attrition, teacher sense of efficacy.',
    'If scripting is necessary for systematic instruction (teachers deviate without it), the extraction is coordination cost. If teachers can deliver systematic phoneme-grapheme instruction without scripting, and scripting reduces teacher effectiveness, the extraction is unjustified and should be removed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_from_teacher_autonomy, empirical, 'Whether teacher autonomy loss through scripting is necessary for systematic instruction or an unjustified extraction.').

omega_variable(
    student_motivation_cost_hidden,
    'What is the cumulative cost in intrinsic reading motivation and engagement from universal application of phoneme-grapheme-first sequencing to students who do not have phonological weakness?',
    'Longitudinal measurement of reading motivation, voluntary reading engagement, and reading identity (self-perception as reader) for students with varying baseline phonological awareness, comparing those in phoneme-grapheme-first vs. balanced vs. meaning-first classrooms.',
    'High motivation costs for strong readers would indicate the constraint is overextended (should target weak readers only, not universal). If motivation costs exist but are offset by decoding gains, that trade-off should be explicit in policy rather than hidden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_motivation_cost_hidden, empirical, 'The intrinsic reading motivation and engagement cost from universal phoneme-grapheme-first sequencing for students without phonological weakness.').

omega_variable(
    kernel_reading_authority_enactment,
    'How did the phonics-reading reading become institutionally dominant (state standards, textbook adoption, teacher training) relative to competing readings, and was that enactment based on evidence strength or institutional power?',
    'Historical analysis of state standard adoption timelines, funding patterns for phonics research vs. balanced/whole-language research, curriculum publisher market concentration, and teacher training shifts. Comparison to evidence meta-analysis timelines.',
    'If enactment was driven by evidence strength (research consensus preceded policy adoption), the institutional power is justified. If enactment was driven by institutional actors (publishers, foundations, policy makers) prior to or independent of evidence consensus, the suppression of alternative readings is extraction, and alternatives should be re-seated in policy discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_authority_enactment, conceptual, 'Whether the phonics-reading institutional dominance reflects evidence strength or institutional power accumulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__phonics_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__phonics_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__phonics_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, information_standard).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__phonics_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy-acquisition kernel decomposes into four constraint readings, each instantiating a different causal model of decoding acquisition. The phonics-reading (this file) asserts phoneme-grapheme decoding PRECEDES and ENABLES comprehension. Whole-language asserts phonological skills EMERGE FROM meaningful text engagement. Balanced-literacy asserts phoneme-grapheme AND meaningful engagement are COMPLEMENTARY. Structured-literacy asserts decoding is ONE OF FIVE essential components (phonological awareness, phonics, fluency, vocabulary, comprehension), designed for dyslexia but universal. These are not views of a single constraint; they are DIFFERENT constraints with different epsilon values (extractiveness), different beneficiary/victim structures, and different kernel interpretations. The phonics-reading reading (this file) has ε=0.68 (moderate-high extraction via teacher autonomy loss). The whole-language reading would have low ε (no scripted suppression) but different victims (students with dyslexia lacking systematic phonological scaffolding). The balanced-literacy reading would have ε between phonics and whole-language, with different extraction targets (teacher judgment constrained but not eliminated). The structured-literacy reading would decompose further into five separate constraint stories (phonological-awareness priority, phonics priority, fluency priority, vocabulary priority, comprehension priority), each with its own epsilon. This file author chose to instantiate the phonics-reading reading as a single kernel reading because that is what the manifest requested; siblings are separate JSON files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__phonics_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
