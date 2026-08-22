% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__whole_language_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Model
 *   domain: educational_psychology/literacy_pedagogy
 *
 * SUMMARY:
 *   The whole language reading framework holds that reading acquisition
 *   emerges naturally from meaningful engagement with connected, authentic
 *   text; that phonics skills develop incidentally through exposure and
 *   context; and that explicit decoding instruction is unnecessary and
 *   potentially harmful to reading motivation. This reading instantiates one
 *   interpretation of the literacy-acquisition kernel: a contested commitment
 *   about how children learn to read. The whole language reading preserves
 *   teacher autonomy and professional judgment while simultaneously
 *   extracting from students whose home environments lack print exposure and
 *   from students with dyslexia whose neurological profile requires explicit
 *   phonological instruction. The claim/metric gap is intentional: the
 *   constraint is claimed as rope (genuine coordination of student motivation
 *   and teacher autonomy) while the authored metrics describe increasingly
 *   extractive operation as the need for explicit suppression (exclusion of
 *   structured literacy approaches, pathologization of dyslexia as motivation
 *   deficit) increases.
 *
 * KEY AGENTS:
 *   - progressive_teachers: beneficiary/agenda-setter (preserve autonomy + professional identity)
 *   - teacher_educator_cohort: beneficiary (institutional reach, career advancement, ideological dominance)
 *   - students_without_print_rich_homes: victim (extraction via inequality assumption)
 *   - students_with_dyslexia_unidentified: victim (identity-locked; pathologized as unmotivated)
 *   - structured_literacy_researchers: excluded (evidence suppressed, framing rejected)
 *   - parents_dyslexia_advocacy: excluded (excluded from curriculum decisions)
 *   - classroom_teachers_ambivalent: observer (constrained, incomplete institutional power)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.72).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Acquisition Model").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, 'fee340ad-f2d6-4d7e-a14d-b4787cd85568').
narrative_ontology:cs_kernel_codification('fee340ad-f2d6-4d7e-a14d-b4787cd85568', distributed).
narrative_ontology:cs_authority_grounding('fee340ad-f2d6-4d7e-a14d-b4787cd85568', lineage).
narrative_ontology:cs_interpretation_layer_present('fee340ad-f2d6-4d7e-a14d-b4787cd85568').
narrative_ontology:cs_reading_relation('fee340ad-f2d6-4d7e-a14d-b4787cd85568', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('fee340ad-f2d6-4d7e-a14d-b4787cd85568', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('fee340ad-f2d6-4d7e-a14d-b4787cd85568', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('fee340ad-f2d6-4d7e-a14d-b4787cd85568', foundational, reading_emerges_from_meaning_engagement).
narrative_ontology:cs_axiom_status(reading_emerges_from_meaning_engagement, holdable).
narrative_ontology:cs_axiom_grounding('fee340ad-f2d6-4d7e-a14d-b4787cd85568', reading_emerges_from_meaning_engagement, empirically_contingent).
narrative_ontology:cs_axiom('fee340ad-f2d6-4d7e-a14d-b4787cd85568', foundational, explicit_phonics_instruction_unnecessary_harmful).
narrative_ontology:cs_axiom_status(explicit_phonics_instruction_unnecessary_harmful, holdable).
narrative_ontology:cs_axiom_grounding('fee340ad-f2d6-4d7e-a14d-b4787cd85568', explicit_phonics_instruction_unnecessary_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('fee340ad-f2d6-4d7e-a14d-b4787cd85568', naturalistic_language_acquisition_model).
narrative_ontology:cs_drift_state('fee340ad-f2d6-4d7e-a14d-b4787cd85568', contemporary_cognitive_neuroscience_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fee340ad-f2d6-4d7e-a14d-b4787cd85568', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, progressive_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teacher_educator_cohort).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_print_rich_homes).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia_unidentified).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement whole language pedagogy and report student engagement and meaning-making as evidence of success. Their professional autonomy and identity are preserved: they are trusted to exercise judgment about authentic literature selection, student interest, and the pace of exposure. The approach validates their intuitions about student motivation and aligns with progressive educational philosophy they hold dear. Training, curriculum materials, and ideological support flow toward this reading; alternative approaches are characterized as mechanical, joyless, developmentally inappropriate.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, progressive_teachers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, progressive_teachers, agenda_setter).

% Depend on schools for exposure to connected text and modeling of fluent reading. The whole language model assumes they will absorb phonemic awareness and decoding through immersion in authentic, meaningful text — an assumption that requires substantial home print exposure, bedtime stories, parent reading aloud, and family conversations about books. Without this background, they fall further behind as peers with print-rich homes extract greater benefit from incidental learning. They cannot exit: school is mandatory, alternative instructional approaches are not available in their district, and home environment is a given.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_print_rich_homes, payer,
    powerless, biographical, trapped, local).

% Have neurological differences in phonological processing that make implicit phonics learning fail while peers succeed. The whole language model attributes their struggle to lack of engagement or motivation, pathologizing them as reluctant readers rather than directing them toward explicit, systematic instruction they need. By the time they are identified (often years later, after shame and avoidance have accumulated), they have internalized a reader identity as broken or incapable. Exit is identity-locked: the reading failure becomes constitutive of their self-concept, and formal identification carries stigma.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia_unidentified, payer,
    powerless, biographical, identity_locked, local).

% Teach courses in reading pedagogy using whole language frameworks, publish articles defending the approach, train new teachers in progressive methods. Their career advancement, publication venue access, and influence over teacher licensure programs depend on the continued dominance of this reading. They have institutional reach and can shape what counts as legitimate pedagogy in university teacher-prep programs. They can shift to different frameworks if necessary (mobile exit), but do not face pressure to do so because institutional support is strong.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teacher_educator_cohort, beneficiary,
    institutional, generational, mobile, national).

% Produce empirical evidence from cognitive science and intervention studies showing phonological awareness, explicit phonics instruction, and cumulative structure improve outcomes, especially for students with dyslexia. Their research findings are publicly available but are excluded from teacher-prep curricula, pedagogical journals, and district professional development. When their work is cited, it is characterized as reducing reading to mechanics, ignoring student engagement, or being based on outdated theories. They cannot exit the field (they are embedded in research institutions), but their influence is blocked.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, structured_literacy_researchers, excluded,
    organized, generational, constrained, national).

% Advocate for identification of dyslexia and access to structured literacy instruction. They have grown in visibility and political capital but are still largely excluded from curriculum decisions at the classroom and district level. Teachers report that dyslexia advocacy messages are conflated with pathologization or medicalization of reading differences. Advocacy groups have constrained exit: they can pressure for policy change but cannot directly alter instruction in the classroom.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_advocacy_dyslexia_organizations, excluded,
    moderate, biographical, constrained, national).

% Teach using whole language frameworks but observe that some students thrive with authentic text while others fall behind despite high engagement. They have limited access to alternative instructional tools, no permission to use explicit phonics sequences without principal approval, and professional liability if they deviate from district-mandated approaches. They occupy an analytical seat: they see the constraint operating but lack power to change it.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, classroom_teachers_ambivalent, observer,
    moderate, biographical, constrained, local).

% Make curricular and accountability decisions based on pedagogical consensus, which has been substantially shaped by the dominance of progressive teacher education. They can commission research, mandate reading approaches, or mandate dyslexia screening and intervention, but they typically defer to educator consensus and move cautiously to avoid ideological conflict.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, policy_makers_state_education, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, teacher_educator_cohort).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of reading instruction motivation and student engagement: by grounding reading in meaningful, authentic, interest-driven text rather than decontextualized skill drills, the approach aligns classroom literacy practice with how children naturally acquire language. Coordinates teacher autonomy with student agency.
% TRANSFER_FUNCTION: Moves legitimacy, classroom authority, and resource allocation from explicit phonics instruction toward implicit, incidental learning through immersion. Transfers professional prestige and curriculum influence toward teachers and educators who adopt progressive frameworks; away from structured literacy researchers and dyslexia specialists. Transfers instructional time and attention away from students without print-rich backgrounds (who would need explicit instruction to overcome the gap) toward students with sufficient home literacy support.
% ABSENT_VOICES: Cognitive scientists studying phonological processing and dyslexia are structurally excluded from curriculum decision-making. Parents whose children have identified or unidentified dyslexia are excluded from the initial framing of what counts as successful reading acquisition. Classroom teachers who observe students failing to decode despite high engagement are excluded from the conversation about why those students fail; their observations are reframed as insufficient motivation or lack of authentic text exposure rather than insufficient explicit instruction.
% DISAPPEARANCE_RATIONALE: If the whole language reading mandate and its enforcement dissolved overnight, classroom instruction would shift toward explicit phonics sequences and cumulative structured literacy approaches; resource allocation to teacher training would redirect to phonological awareness and decoding instruction; diagnostic identification of dyslexia would increase (students would be evaluated for learning differences rather than motivation deficits); teacher professional identity would reorient from meaning-maker and engagement facilitator toward explicit skill instructor. The ecosystem of publishing, curriculum materials, and teacher-prep programs would reorganize.
% FOUNDING_PROBLEM: Early-1980s reading instruction relied heavily on phonics worksheets, decontextualized word lists, and dull basals that produced student boredom and mechanical reading without comprehension. Whole language emerged to restore meaning, engagement, and authentic literature to the classroom, asking whether reading skill could develop naturally through immersion in rich, purposeful, enjoyable text.
% FOUNDING_PROBLEM_CORROBORATION: Progressive educators attest that the founding problem (student disengagement from mechanical phonics) remains live and that whole language solves it by restoring joy. Cognitive researchers and dyslexia advocates attest the founding problem is substantially solved by modern basal materials, and that the whole language response created a new problem: students without home literacy support and students with dyslexia fall further behind. Longitudinal reading-score data from jurisdictions that shifted from phonics-heavy to whole language approaches shows declining performance, particularly for disadvantaged students; this is contested by progressive educators as an artifact of transition or measurement insensitivity to engagement gains.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.42 to 0.68 over the interval as the constraint hardens and alternative approaches are increasingly deligitimized. At t=0, the constraint is younger and coexists more easily with phonics-informed practice; by t=40, whole language dominance is thorough and suppression of alternatives is routine. Theater ratio rises from 0.22 to 0.41 but plateaus: the engagement-and-meaning function is real and performs, but a growing share of enforcement effort is devoted to excluding structured literacy and pathologizing dyslexia advocates rather than delivering the coordination benefit itself. Suppression requirement (the active force needed to exclude alternatives) rises from 0.48 to 0.72 as the evidence for structured literacy accumulates and must be suppressed more forcefully. Accessibility collapse (0.58) reflects that alternatives (explicit phonics, structured literacy) exist and are known, but are framed as harmful to motivation and development; for teachers within the progressive cohort, exit from whole language is constrained by professional identity and institutional incentives, even though alternatives are not made physically unavailable. Resistance (0.74) is high because cognitive scientists, dyslexia researchers, and parents of struggling readers all contest the framework; the constraint persists not by consensus but by institutional gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   Classroom teachers positioned in a dual role (observer + constrained implementer) experience the constraint differently from both beneficiary and victim seats. They see the real engagement gains and the real struggles of students without home literacy support in the same classroom. Their constrained exit (no permission to use alternative approaches without district approval, professional liability for deviation) means they cannot resolve the tension by switching frameworks. They occupy the seat where the constraint's contradictions are most visible and least resolvable.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive teachers have low directionality (d ~0.2-0.3): they benefit from preserved autonomy, ideological validation, and professional respect; they bear no cost from the constraint (they are the agenda-setters). Teacher educators have similar low d (d ~0.15-0.25): they benefit institutionally and can shift if needed (mobile exit). Students without print-rich homes have high directionality (d ~0.85-0.95): they bear substantial costs (falling behind peers, internalized failure), have no exit (trapped), and no beneficiary status (they gain engagement but do not gain the decoding skills they need). Unidentified students with dyslexia have the highest directionality (d ~0.9+): they bear extraction (pathologized as unmotivated), are identity-locked (reader identity becomes constitutive of self-concept), and receive no coordination benefit (meaning-making fails when phonological processing is impaired). Structured literacy researchers have moderate-high directionality (d ~0.6-0.7): they bear a cost (excluded, delegitimized) but have some professional autonomy and can publish in alternative venues; the primary cost is blocked influence, not direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (student disengagement from mechanical phonics instruction) is contested. Progressive educators attest it is still live; cognitive researchers attest it is substantially solved by modern basals and motivation comes from a mix of intrinsic interest and explicit instruction that builds competence. The divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges raises a mandatrophy flag: the constraint persists because institutional beneficiaries (teacher educators, progressive teachers) have gatekeeping power, not because the founding problem remains unresolved. If the founding problem were truly the driver, we would expect measured reading engagement to remain high as extractiveness rose; instead, the measurement series show extractiveness rising even as engagement claims are made (theater_ratio rising, suggesting performance of engagement without full functional delivery). This is a mandatrophy candidate: a constraint whose stated function (solve the problem of reading disengagement) has been substantially addressed, but which persists due to institutional inertia and beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_vs_explicit_phonics_learning,
    'Can phonological awareness and phoneme-grapheme correspondence develop sufficiently through incidental exposure to text, or does efficient decoding require explicit, systematic instruction?',
    'Longitudinal studies comparing reading outcomes in classrooms using primarily whole language (implicit) versus balanced or structured approaches (explicit instruction); subgroup analysis for students with varying home literacy exposure and for students with identified dyslexia or phonological processing differences.',
    'If implicit learning is sufficient across all subgroups, whole language is vindicated as coordination achieving both engagement and competence. If implicit learning fails for subgroups without home literacy support or with processing differences, the constraint is revealed as extractive from those subgroups and the claimed coordination function is revealed as conditional on prior advantage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_vs_explicit_phonics_learning, empirical, 'Whether phonics acquisition requires explicit instruction or emerges implicitly from text immersion.').

omega_variable(
    home_literacy_assumption_visibility,
    'Is the whole language framework''s reliance on home print exposure (bedtime stories, family reading, print-rich environment) explicitly acknowledged and accounted for in classroom implementation, or is it treated as a universal given?',
    'Analysis of whole language curriculum materials and teacher-training texts to measure explicit acknowledgment of socioeconomic literacy-exposure gaps; classroom observations to measure whether teachers adjust scaffolding or supplemental instruction based on student home literacy background; outcome data disaggregated by home literacy exposure.',
    'If the assumption is invisible and unaccounted for, the constraint extracts from students without home literacy support by design. If the assumption is visible and supplemental instruction is provided, the extraction is mitigated (though the underlying inequality is still reproduced). If outcomes are equal across home literacy backgrounds within whole language classrooms, the assumption''s impact is negligible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(home_literacy_assumption_visibility, empirical, 'Whether the whole language framework acknowledges and accounts for home literacy exposure as a prerequisite.').

omega_variable(
    dyslexia_identification_pathologization,
    'When students fail to develop decoding skills despite whole language instruction and high engagement, are they evaluated for specific learning differences (dyslexia, phonological processing deficit), or are they attributed to motivation/engagement deficits?',
    'Comparative data on dyslexia identification rates in whole language versus structured literacy jurisdictions; qualitative analysis of how struggling readers are characterized in teacher reports and parent conferences; age of identification for dyslexic students across different instructional approaches.',
    'If students without processing deficits are mislabeled as unmotivated and left without appropriate instruction, the constraint extracts through identity-locking (internalized reader failure). If students are promptly identified and given appropriate instruction, the extraction is mitigated. Early identification correlates with better long-term outcomes even in whole language classrooms, suggesting suppression of dyslexia identification is extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dyslexia_identification_pathologization, empirical, 'Whether reading failure is attributed to motivation deficits or to neurological differences requiring explicit instruction.').

omega_variable(
    kernel_reading_contestation,
    'Is the whole language reading a legitimate permanent interpretation of the literacy-acquisition kernel, or is it a reading that has been superseded by accumulating evidence about phonological processing and explicit instruction?',
    'Meta-analysis of experimental studies on reading instruction approaches and their outcomes; analysis of cognitive neuroscience findings on phonological processing; tracking of changes in major educational organizations'' recommendations (ILA, American Academy of Pediatrics, etc.) over time.',
    'If whole language represents a stable, empirically supported reading that coexists legitimately with structured and balanced literacy readings, the readings should continue to coexist. If the evidence has decisively shown that explicit instruction improves outcomes (especially for disadvantaged students and those with dyslexia), the whole language reading''s foundational axiom (explicit instruction is unnecessary/harmful) is empirically overridden. An overridden axiom does not foreclose the reading (teachers can still choose to adopt it), but it changes the epistemic standing of the reading from an open question to a defeated position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Whether the whole language reading represents a live pedagogical position or a position whose core empirical premises have been substantially challenged.').

omega_variable(
    teacher_autonomy_vs_institutional_gatekeeping,
    'Does the whole language framework genuinely preserve teacher professional autonomy and judgment, or does it restrict autonomy to teachers who accept the whole language premises while suppressing those who want to use structured literacy approaches?',
    'Survey of classroom teachers about their instructional freedom and pressure to conform; analysis of curriculum materials and textbooks approved/rejected by districts; measurement of professional consequences (evaluation ratings, hiring, advancement) for teachers who adopt alternative approaches within whole language-dominant districts.',
    'If whole language preserves teacher autonomy universally, it is coordination achieving both engagement and professional respect. If it restricts autonomy to whole language practitioners while suppressing alternatives, the low extractiveness on teachers (beneficiary seat) is contingent on ideological conformity, and the constraint includes institutional gatekeeping of educational approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_institutional_gatekeeping, empirical, 'Whether teacher autonomy is genuinely preserved or contingent on ideological conformity to whole language frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__whole_language_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy-acquisition kernel decomposes into four structurally distinct constraints: whole_language_reading (this story), phonics_reading, balanced_literacy_reading, and structured_literacy_reading. Each reading instantiates a different claim about how reading acquisition works, which has different ε values, beneficiary/victim structures, and enforcement requirements. The whole language reading claims reading emerges from meaning and engagement (low explicit instruction overhead, high extraction from students without home literacy support). The structured literacy reading claims reading requires explicit, cumulative, multimodal instruction (high overhead, lower extraction from disadvantaged students because explicit instruction bridges the gap). The phonics reading claims decoding precedes comprehension and requires systematic phoneme-grapheme instruction. The balanced reading claims both systematic phonics and meaningful engagement are necessary. These are not the same constraint viewed from different angles — they have different ε values (whole language ε≈0.68, structured literacy ε≈0.35-0.42), different victim sets (whole language victims = students without home literacy + unidentified dyslexics; structured literacy victims = teachers whose autonomy is restricted by mandated curriculum), and different beneficiary structures. The kernel is the commitment: 'how do children learn to read?' The readings are institutionalized interpretations of that kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__whole_language_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
