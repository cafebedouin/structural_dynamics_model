% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Explicit Phonics-First Reading Instruction Mandate
 *   domain: educational/cognitive/pedagogical
 *
 * SUMMARY:
 *   The phonics-reading instantiates one interpretation of how reading
 *   acquisition works: explicit, systematic instruction in phoneme-grapheme
 *   correspondence is both necessary and sufficient for reading competence;
 *   decoding skill is foundational and precedes comprehension. This reading
 *   frames early literacy as a skill-building problem with a known technical
 *   solution, accessible through structured, sequence-driven instruction. The
 *   constraint's beneficiaries are students with weak phonological awareness
 *   (who genuinely struggle without systematic decoding support) and students
 *   with dyslexia (for whom the Orton-Gillingham-derived phonological
 *   approach is empirically supported). The constraint's victims are teachers
 *   (whose professional autonomy is scripted away) and the construct 'teacher
 *   professional judgment' (which is structural casualties of fidelity
 *   enforcement). The measurement series shows rising extractiveness over the
 *   first 15 time points (as the mandate moves from policy to classroom
 *   implementation) and then plateaus, indicating the constraint has
 *   stabilized into institutional practice. Rising theater ratio indicates
 *   growing emphasis on performative fidelity metrics (lesson observations,
 *   curriculum adherence checklists) independent of student literacy
 *   outcomes.
 *
 * KEY AGENTS:
 *   - Students with weak phonological awareness: beneficiaries receiving explicit, systematic decoding instruction
 *   - Dyslexic students: beneficiaries for whom phoneme-centric instruction is structurally necessary
 *   - Classroom teachers: payers bearing loss of real-time instructional autonomy and increased surveillance
 *   - Reading scientists (phonics camp): agenda-setters framing decoding-centric evidence as empirical consensus
 *   - Curriculum publishers (structured literacy): beneficiaries gaining guaranteed adoption markets
 *   - Whole-language educators: excluded from curriculum adoption and policy formation
 *   - Policymakers/school administrators: agenda-setters enforcing fidelity and accountability metrics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.71).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Explicit Phonics-First Reading Instruction Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational/cognitive/pedagogical").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '91d6a693-29f9-4f1b-bb73-2751f5b400b8').
narrative_ontology:cs_kernel_codification('91d6a693-29f9-4f1b-bb73-2751f5b400b8', distributed).
narrative_ontology:cs_authority_grounding('91d6a693-29f9-4f1b-bb73-2751f5b400b8', extraction).
narrative_ontology:cs_interpretation_layer_present('91d6a693-29f9-4f1b-bb73-2751f5b400b8').
narrative_ontology:cs_reading_relation('91d6a693-29f9-4f1b-bb73-2751f5b400b8', literacy_acquisition_kernel__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('91d6a693-29f9-4f1b-bb73-2751f5b400b8', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('91d6a693-29f9-4f1b-bb73-2751f5b400b8', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('91d6a693-29f9-4f1b-bb73-2751f5b400b8', foundational, systematic_phoneme_instruction_foundational).
narrative_ontology:cs_axiom_status(systematic_phoneme_instruction_foundational, holdable).
narrative_ontology:cs_axiom_grounding('91d6a693-29f9-4f1b-bb73-2751f5b400b8', systematic_phoneme_instruction_foundational, empirically_contingent).
narrative_ontology:cs_axiom('91d6a693-29f9-4f1b-bb73-2751f5b400b8', foundational, decoding_precedes_comprehension_logically).
narrative_ontology:cs_axiom_status(decoding_precedes_comprehension_logically, holdable).
narrative_ontology:cs_axiom_grounding('91d6a693-29f9-4f1b-bb73-2751f5b400b8', decoding_precedes_comprehension_logically, deontological).
narrative_ontology:cs_reference_frame('91d6a693-29f9-4f1b-bb73-2751f5b400b8', phoneme_instruction_sequence_established).
narrative_ontology:cs_drift_state('91d6a693-29f9-4f1b-bb73-2751f5b400b8', contemporary_practice_divergence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91d6a693-29f9-4f1b-bb73-2751f5b400b8', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, struggling_early_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, dyslexic_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, teacher_professional_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_structured_literacy).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_from_literacy_rich_homes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive explicit, structured instruction in phoneme-grapheme mapping that makes decoding transparent and learnable. Without this systematic sequence, these students face persistent reading failure and often internalize a fixed-mindset belief about their reading ability. The systematic progression prevents the accumulation of unaddressed skill gaps that cascade through subsequent grades.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, national).

% Benefit from decoding-before-comprehension sequencing because it removes ambiguity about how to extract sound from print. They experience earlier success with decodable texts, building confidence and fluency before encountering literature with irregular orthography.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, struggling_early_readers, beneficiary,
    powerless, biographical, trapped, national).

% Require explicit, cumulative phonological instruction as a structural accommodation; for this population, meaning-first approaches systematically fail. The phonics-first mandate operationalizes the finding that dyslexic learners need orthographic mapping made explicit to rewire phonological processing.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, dyslexic_students, beneficiary,
    powerless, biographical, trapped, national).

% Implement scripted, sequenced phonics curricula that remove instructional judgment about pacing, progression, and content selection. They bear the cost of loss of pedagogical autonomy and face surveillance (lesson observations, fidelity audits) to ensure compliance. Some teachers experience this as deskilling and professionally demoralized; others experience scripting as liberating (removing uncertainty about what to teach). The constraint operates differently depending on the teacher's pre-mandate instructional practice and epistemic frame.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__phonics_reading, classroom_teachers, agenda_setter).

% The capacity to adjust instruction in real time based on student response, to select texts that motivate and engage specific learner communities, to make diagnostic inferences about reading difficulty independent of the scripted sequence. This autonomy is structurally constrained by the mandate's fidelity requirements.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, teacher_professional_autonomy, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_non_agent(literacy_acquisition_kernel__phonics_reading, teacher_professional_autonomy).

% Sell structured literacy and phonics-based curricula (Fundations, Wilson Reading System, Orton-Gillingham adaptations, Phonics First) to school districts implementing the mandate. The mandate creates guaranteed market demand for their products and reduces competition from meaning-first or balanced-literacy publishers.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_structured_literacy, beneficiary,
    institutional, generational, arbitrage, global).

% Frame decoding-centric reading science (cognitive load theory, orthographic mapping, phonological deficit hypothesis) as consensus empirical fact and architect policy/mandate language that operationalizes this reading of the evidence. They hold interpretive authority over what 'science-based reading instruction' means, and the mandate embeds their frame into law and practice.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_scientists_phonics_camp, agenda_setter,
    institutional, generational, arbitrage, global).

% Would argue that meaning-centered engagement with authentic texts supports reading development and that systematic phonics instruction undermines motivation and treats reading as a mechanical code-cracking exercise. They are excluded from curriculum adoption decisions and teacher training, and their published research on literature-based and meaning-first approaches is reframed in mandate discourse as pedagogically harmful.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_educators, excluded,
    moderate, biographical, constrained, national).

% Implement instruction that integrates systematic phonics WITH meaningful text and student choice, arguing that both components are necessary and that the sequence is learner-responsive rather than universally fixed. The mandate's scripted, phonics-before-meaning requirement forecloses this hybrid approach and treats integrated instruction as insufficiently rigorous or evidence-based.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, balanced_literacy_practitioners, excluded,
    moderate, biographical, constrained, national).

% Often benefit from explicit phonics instruction but would succeed in a variety of instructional approaches because they arrive at school with rich oral language, phonological awareness, and print exposure. The mandate is partly defensive against them (their success was the cover story for meaning-first instruction); partly beneficial (ensuring their phonological skills are formalized, not assumed).
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_from_literacy_rich_homes, beneficiary,
    moderate, biographical, mobile, national).

% Enforce the phonics mandate through curriculum adoption, professional development requirements, lesson observations, and fidelity metrics. They face pressure from reading scientists (who claim the science is settled), policymakers (who legislate the mandate), parents of struggling readers (who demand systematic intervention), and teachers (who resist deskilling). Administrators operationalize the mandate as a structural requirement independent of local context.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, school_administrators, agenda_setter,
    powerful, generational, constrained, national).

% Legislate or mandate phonics-first instruction through science of reading laws, teacher training requirements, and accountability frameworks tied to reading achievement data. The mandate is presented as protecting vulnerable students and fixing reading failure; it is enforced through funding contingencies and teacher evaluation metrics.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, policymakers_accountability_state, agenda_setter,
    institutional, generational, constrained, national).

% Produces evidence that the mandate interprets and frames. The phonics-reading observes the constraint's operation and produces measurement data (literacy outcome data, reading fluency metrics, comprehension assessments) that the mandate uses to adjudicate its own performance. Some researchers question the reading's framing of the evidence; they are marginalized as ideologically opposed to science or protecting failed approaches.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_research_community_contested, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_structured_literacy).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the early-literacy skill-building problem for populations with weak phonological awareness by making decoding instruction explicit, sequential, and standards-based. Ensures a common, systematic progression through phoneme-grapheme correspondences before students encounter irregular text or meaning-centered reading. Creates a shared framework so that all teachers, regardless of prior training or implicit theories, deploy the same skill sequence.
% TRANSFER_FUNCTION: Transfers instructional autonomy and professional judgment from individual teachers to mandated curricula and scripted sequences. Teachers cede real-time pedagogical decision-making (when to introduce new phonemes, which texts to use for practice, whether a student needs more time on a particular correspondence) in exchange for systematic organization and (theoretically) improved outcomes for struggling readers. Curriculum publishers and reading scientists benefit from guaranteed market adoption and policy-institutionalized authority. Students with weak phonological awareness receive explicit instruction; students from literacy-rich homes experience less individualized engagement.
% ABSENT_VOICES: Whole-language educators and balanced-literacy practitioners are structurally excluded from curriculum adoption, policy formation, and professional development. They would argue that the mandate's strict phoneme-first, decodable-text-only sequence undermines motivation, reduces engagement with authentic literature, and treats reading as a mechanical exercise rather than a meaning-making one. Teachers who integrate systematic phonics with student choice and meaningful text are positioned as insufficiently evidence-based. Students who thrive in literature-rich, meaning-centered environments (including students of color in communities with strong oral-tradition literacy practices) are not present in mandate discourse.
% DISAPPEARANCE_RATIONALE: If the phonics mandate disappeared, schools would revert to a mix of instructional approaches—some returning to balanced literacy, some to meaning-first engagement, some to teacher-designed hybrid sequences. Students with weak phonological awareness would face re-exposure to unstructured, inconsistent instruction and likely experience returning reading gaps. Curriculum publishers would lose guaranteed adoption revenue. Teachers would regain instructional autonomy and reduce fidelity audits. The elimination would immediately raise contention about whether reading instruction is a science problem (settled by neuroscience) or an art problem (requiring teacher judgment).
% FOUNDING_PROBLEM: Early reading failure, particularly for students with dyslexia and weak phonological awareness, persisted despite decades of teacher training and meaning-centered instruction. Students who could not decode by sound were trapped in reading failure, unable to access comprehension and knowledge. The organizing claim: explicit instruction in phoneme-grapheme mapping directly addresses the decoding deficit that blocks reading acquisition.
% FOUNDING_PROBLEM_CORROBORATION: Reading scientists (Dehaene, Moats, Scarborough) and neuroscience-informed researchers attest that the phonological deficit is foundational and requires explicit remediation; intervention studies on phonics-based programs (DIBELS benchmarks, Wilson Reading outcomes) show improved decoding for explicitly taught phoneme patterns. Teachers and whole-language researchers contest that the founding problem overstates the gap and ignores students who develop reading fluency through meaning-centered approaches; they cite longitudinal studies and case evidence from literature-rich classrooms where all students, including those with weak phonological awareness, develop reading competence without front-loaded phoneme instruction. Policymakers cite reading failure rates and grade-level retention as evidence the founding problem is live. Independent meta-analyses (National Reading Panel, Rose Report, NRP-update reviews) are divided on whether phoneme-first sequencing is necessary for all students or primarily beneficial for students with identified phonological deficits.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 endpoint) because the constraint extracts instructional autonomy from teachers and concentrates interpretive authority (what 'science-based reading instruction' means) in the reading scientists and publishers who authored the phonics frame. Teachers face surveillance (fidelity audits, lesson observations) to ensure compliance; they cannot adjust pacing or select texts based on student response without risking accountability consequences. Suppression is high (0.71) because the mandate operates through accountability infrastructure: teachers who do not adhere to scripted sequences face evaluation penalties, students cannot be placed in meaning-centered or balanced-literacy settings without pressure to return to phonics-first, and alternative research is reframed as ideologically motivated or outdated. Theater ratio rises from 0.25 to 0.42 over the interval, indicating a shift: early in implementation (t0), the constraint is primarily functional (teachers learning and applying a new instructional method); by t25, growing emphasis on compliance documentation, fidelity checklists, and reading-level data collection (often disconnected from whether individual students are reading widely or with engagement) suggests performative maintenance of the mandate independent of actual literacy outcome gains. Accessibility collapse is moderate (0.62): alternatives (whole-language, balanced literacy, student-directed reading) exist and remain live in teachers' awareness, but they are not available within the mandate's constraints—exit requires either changing schools, working outside the curriculum, or leaving the profession. Resistance is moderate (0.58): teachers push back through informal adaptation (supplementing decodable texts with literature, loosening fidelity to allow student choice), through union advocacy for autonomy, and through continued published debate from balanced-literacy and whole-language researchers—but the resistance does not yet destabilize the mandate's institutional grip.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (struggling students with weak phonological awareness) and the payer seats (teachers) experience this constraint at opposite directionalities. From the student's perspective (especially students with dyslexia), the constraint is life-changing: explicit, sequential phoneme instruction finally makes decoding teachable and learnable, lifting a barrier that meaning-centered approaches left in place. From the teacher's perspective, the constraint is a loss of professional judgment and an increase in compliance burden; teachers report feeling deskilled and observed, and their capacity to respond to individual learner variation is constrained by scripting. The payer's directionality (d high, near 1.0) should compute a Snare-like type at the teacher seat; the beneficiary's directionality (d low, near 0.0) should compute a Mountain-like or Rope-like type at the struggling-student seat. The engine should compute both, showing the constraint's type diverges by seat—a tangled-rope structure with genuine coordination benefit (decoding skill-building works) AND asymmetric extraction (teacher autonomy is the cost borne by a different set of stakeholders). The secondary_role pairing on classroom_teachers (both payer and agenda_setter) captures the dual position: teachers enforce the mandate they also experience as constraining—a nested asymmetry within a single institutional seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers with weak phonological awareness are the structural beneficiaries: they receive explicitly scaffolded instruction that removes the cognitive load of figuring out decoding rules implicitly. Their directionality is low (d ≈ 0.1–0.2): they benefit substantially (early skill-building success, reduced reading failure), have minimal exit options (trapped in the reading difficulty; the instruction is offered to them, not optional), and experience the constraint as enabling. Teachers are the structural payers: they lose real-time instructional judgment, face surveillance through fidelity audits, and experience deskilling (the constraint replaces their professional inference with scripted sequences). Their directionality is high (d ≈ 0.75–0.85): they bear the autonomy cost, can exit only through changing careers or schools (constrained), and experience the constraint as coercive even when they cognitively endorse the phonics evidence. Reading scientists and curriculum publishers are beneficiaries with organized power: they gain institutional authority and market revenue. Their directionality is low (d ≈ 0.05–0.15). Students from literacy-rich homes are asymmetrically positioned: they benefit from the systematic phoneme instruction but would succeed in other approaches too; they experience less loss from narrowed text selection because their home literacy environment compensates. Their directionality is near-symmetric (d ≈ 0.4–0.5): genuine benefit, but not extraction-dependent. Whole-language educators and balanced-literacy practitioners are excluded; they would experience the mandate as victimization (foreclosure of their instructional approaches, reframing of their research as harmful), but are not named as victims in base_properties because they are excluded from the direct constraint structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as Tangled Rope because it possesses both genuine coordination function AND asymmetric extraction. The coordination is real: systematic, explicit phoneme-grapheme instruction does improve decoding skill for students with weak phonological awareness, and decoding skill is foundational to reading comprehension. The extraction is real: teachers' instructional autonomy is systematically constrained, and the constraint persists through active enforcement (accountability measures, surveillance, curriculum mandates) rather than through teacher or student preference. The tangled-rope classification prevents mislabeling this as either a pure Rope (which would ignore the autonomy extraction) or a pure Snare (which would ignore the genuine decoding benefit). The 'tangled' nature is the point: the constraint genuinely solves a coordination problem AND systematically extracts a cost from a different stakeholder set. Mandatrophy resolution: The founding problem is whether early literacy failure in specific populations is a skill-building gap (technical problem, solved by systematic decoding instruction) or a systemic inequality gap (a resource and social capital problem, where meaning-centered engagement in literacy-rich environments prevents failure). If the first is true and decoding skill is foundational for all students, the mandate is a rope solving a real collective-action problem (everyone benefits from systematic progression). If the second is true and many students develop reading competence through meaning-centered approaches, the mandate is a snare using decoding-science framing as a cover story for deskilling teachers and privatizing reading instruction. The contested founding-problem status maps to this ambiguity: reading scientists attest the skill-building frame is empirically true (longitudinal phoneme studies, orthographic mapping evidence); whole-language researchers attest the systemic inequality frame is empirically true (meta-analyses on engagement and motivation, longitudinal case evidence from literature-rich schools with diverse students). The tangled-rope type sits at the intersection: the constraint is neither purely functional nor purely extractive; it contains both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonological_deficit_vs_systemic_inequity,
    'Is early reading failure in specific student populations caused by a phonological processing deficit (a cognitive architecture gap) or by systemic inequity in literacy exposure and engagement (a resource and social capital gap)?',
    'Longitudinal comparison of reading outcomes in meaning-centered, engagement-rich literacy programs serving low-literacy-exposure populations versus phoneme-first programs serving the same populations. If meaning-centered programs produce equivalent or superior outcomes with equivalent investment, the phonological deficit frame is overstated; if phoneme-first programs consistently outperform meaning-centered approaches for weak-phonological-awareness students specifically, the deficit frame is validated.',
    'If systemic inequity is the primary driver, the mandate''s phoneme-first approach may be a technically correct solution to a wrongly-diagnosed problem; the real remedy is literacy-rich classrooms and engagement with authentic texts. If phonological deficit is primary, the mandate is appropriately targeted. The classification changes from Tangled Rope (genuine coordination benefit + extraction cost) to Snare (extraction with a cover story) in the first case, and holds as Tangled Rope in the second.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(phonological_deficit_vs_systemic_inequity, empirical, 'Whether early reading failure is a skill-learning gap or a resource-exposure gap.').

omega_variable(
    scripted_autonomy_loss_vs_liberation_from_uncertainty,
    'Is the loss of real-time instructional autonomy experienced by teachers an extraction (deskilling, surveillance, professional disempowerment) or a liberation from uncertainty and an equalization of instructional quality across teachers with different prior training?',
    'Qualitative longitudinal study of teacher experience pre- and post-mandate, with attention to teachers'' reported sense of agency, confidence, and professional identity. Comparison of instructional quality (measured by phoneme-sequencing consistency, scaffolding appropriateness, student engagement) across high-autonomy, low-structure classrooms versus scripted, high-structure classrooms in comparable student populations. Teacher attrition and career-continuation data to assess whether scripting increases or decreases teacher persistence.',
    'If scripting is experienced as deskilling by most teachers and does not improve instructional consistency, the extraction cost is higher than supposed (more like Snare). If scripting is experienced as liberating by teachers new to literacy instruction and improves consistency without reducing teacher satisfaction, the extraction cost is justified as coordination overhead (holds as Tangled Rope). If scripting produces divergent experiences (some teachers liberated, some deskilled), the constraint''s distributional effects are non-uniform and depend on baseline teacher confidence and training.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scripted_autonomy_loss_vs_liberation_from_uncertainty, empirical, 'Whether autonomy loss is experienced as extraction or as helpful constraint.').

omega_variable(
    kernel_reading_alternative_frames,
    'Does the foundational kernel claim ''reading acquisition requires systematic, explicit phoneme instruction'' refer to an empirical claim about cognitive necessity (decoding skill is cognitively foundational for ALL readers) or a prescriptive claim about instructional design (explicit phoneme instruction is the MOST EFFICIENT way to teach decoding, but other paths exist)?',
    'Close reading of the reading scientists'' published work distinguishing empirical claims (what is true about reading development) from prescriptive claims (what instruction is best for different populations). Examination of whether the evidence base supports a universal claim (phoneme-first is necessary for all students) or a population-specific claim (phoneme-first is necessary for students with dyslexia and weak phonological awareness, beneficial for most others, and compatible with but not required for students from high-literacy backgrounds).',
    'If the claim is empirically universal, the mandate is a justified boundary condition (Rope or Tangled Rope with legitimate universal scope). If the claim is population-specific and prescriptive, the mandate''s application to all students (including those who develop reading competence through other approaches) is overgeneralization and may constitute Snare-class extraction (imposing an inefficient solution on populations for whom it is not necessary). The classification implications are consequential: a universal Mountain-claim would prevent alternatives by necessity; a population-specific claim should permit instruction to be tailored, and universal mandates override this tailoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_frames, conceptual, 'Whether the phoneme-instruction necessity claim is universal or population-specific.').

omega_variable(
    measurement_basis_outcome_vs_fidelity_drift,
    'As theater_ratio rises over the measurement interval (0.25 to 0.42), does the rising ratio reflect a genuine increase in performative-over-functional activity (Goodhart drift, where fidelity metrics replace learning outcomes), or does it reflect an honest increase in the administrative overhead necessary to maintain instructional consistency as the mandate matures?',
    'Longitudinal study of how classroom time is allocated across phoneme instruction, practice, assessment, and documentation. Measurement of correlation between fidelity-audit compliance and student literacy outcomes; if correlation weakens over time, theater_ratio rise reflects Goodhart drift. If correlation remains strong, the rise reflects maturing administrative infrastructure.',
    'If Goodhart drift is occurring, the constraint is degrading toward Piton status: the original coordination function (systematic decoding skill-building) persists, but a growing share of enforcement energy is devoted to performative compliance metrics decoupled from literacy outcomes. This would support a future reclassification from Tangled Rope to Piton once the coordination function atrophies. If the rise reflects necessary infrastructure, the constraint''s type remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_basis_outcome_vs_fidelity_drift, empirical, 'Whether rising theater_ratio indicates Goodhart drift or maturing administrative infrastructure.').

omega_variable(
    whole_language_sibling_foreclosure_or_coexistence,
    'Does the phonics_reading logically foreclose the whole_language_reading (making both non-simultaneously-holdable in a single coherent framework), or do the two readings coexist as different parties'' live positions in an ongoing unresolved dispute?',
    'Logical analysis of the core axioms: systematic_phoneme_instruction_foundational (phonics) vs. meaningful_text_engagement_foundational (whole_language). If ''foundational'' means ''necessary and sufficient,'' the axioms contradict and foreclose. If ''foundational'' means ''primary'' or ''sufficient for many learners,'' the axioms can coexist in different populations or instructional contexts. Examination of whether any published researcher or educator holds BOTH axioms in integrated form (e.g., ''phoneme instruction is foundational for some students; meaningful engagement is foundational for others'').',
    'If foreclosure holds, the constraint''s type should reflect that one reading has eliminated the other from the coherent-framework space—a structural victory for phonics_reading. If coexistence holds, the readings remain in live dispute, and mandates enforcing one reading are suppression of the other (higher suppression metric, more Snare-ish classification). This omega determines the reading_relations entry for the whole_language_reading sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(whole_language_sibling_foreclosure_or_coexistence, conceptual, 'Whether phonics and whole-language readings are logically contradictory or empirically different.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__phonics_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(lite_tr_t5, observed).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(lite_tr_t10, observed).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__phonics_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(lite_tr_t15, observed).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(lite_tr_t20, observed).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__phonics_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(lite_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(lite_be_t5, observed).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(lite_be_t10, observed).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(lite_be_t15, observed).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(lite_be_t20, observed).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(lite_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(lite_su_t5, observed).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(lite_su_t10, observed).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(lite_su_t15, observed).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(lite_su_t20, observed).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(lite_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__phonics_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel decomposes into four structurally distinct constraint stories, each instantiating a different reading of the foundational kernel commitment about how reading development works. phonics_reading frames reading acquisition as skill-learning (decoding-first, explicit phoneme instruction necessary and foundational); whole_language_reading frames it as meaning-emergence (engagement with authentic text necessary and foundational); balanced_literacy_reading frames it as complementary integration (phonics and meaning both necessary, sequence learner-responsive rather than universal); structured_literacy_reading frames it as cumulative-scaffolded-multimodal instruction (phonological + orthographic + fluency + comprehension in integrated sequence, designed for dyslexia but applicable universally). The four readings coexist in educational policy, research, and practice; they have different beneficiary/victim structures, different ε values, and different strategic interests. Linking them via network.affects_constraints enables contamination analysis: how the cognitive/empirical status of one reading (e.g., evidence accumulation against whole_language) affects the justification and sustainability of another reading (e.g., strengthens mandated phonics). The four readings form the literacy_acquisition_kernel constraint family; all members must be linked to at least one sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__phonics_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
