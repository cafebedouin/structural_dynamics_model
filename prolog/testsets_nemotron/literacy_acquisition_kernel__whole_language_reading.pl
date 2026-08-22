% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Theory
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Whole language reading theory asserts that children acquire reading
 *   naturally through immersion in meaningful, connected text — just as they
 *   acquire oral language. Explicit, systematic phonics instruction is framed
 *   as unnecessary, disruptive to meaning-making, and potentially damaging to
 *   motivation. The theory gained dominance in teacher education and state
 *   curricula from the 1980s through early 2000s. Its coordination function
 *   is real: it replaced fragmented, low-expectation basal programs with a
 *   cohesive, literature-rich vision. But its extraction function is
 *   asymmetric: it assumes a print-rich home environment that many children
 *   do not have, and it actively suppresses alternative instructional
 *   approaches (especially systematic phonics) through professional
 *   gatekeeping, accreditation standards, and curricular control. The
 *   constraint is a tangled rope: genuine coordination around
 *   meaning-centered pedagogy, but with extraction concentrated on students
 *   who need explicit decoding instruction to succeed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Acquisition Theory").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, 'fd0a5531-528d-43d1-8f4b-6ef3710765d5').
narrative_ontology:cs_kernel_codification('fd0a5531-528d-43d1-8f4b-6ef3710765d5', distributed).
narrative_ontology:cs_authority_grounding('fd0a5531-528d-43d1-8f4b-6ef3710765d5', practice).
narrative_ontology:cs_interpretation_layer_present('fd0a5531-528d-43d1-8f4b-6ef3710765d5').
narrative_ontology:cs_reading_relation('fd0a5531-528d-43d1-8f4b-6ef3710765d5', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('fd0a5531-528d-43d1-8f4b-6ef3710765d5', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_reading_relation('fd0a5531-528d-43d1-8f4b-6ef3710765d5', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('fd0a5531-528d-43d1-8f4b-6ef3710765d5', foundational, decoding_instruction_harms_motivation).
narrative_ontology:cs_axiom_status(decoding_instruction_harms_motivation, holdable).
narrative_ontology:cs_axiom_grounding('fd0a5531-528d-43d1-8f4b-6ef3710765d5', decoding_instruction_harms_motivation, deontological).
narrative_ontology:cs_axiom('fd0a5531-528d-43d1-8f4b-6ef3710765d5', foundational, reading_is_meaning_construction_not_decoding).
narrative_ontology:cs_axiom_status(reading_is_meaning_construction_not_decoding, holdable).
narrative_ontology:cs_axiom_grounding('fd0a5531-528d-43d1-8f4b-6ef3710765d5', reading_is_meaning_construction_not_decoding, deontological).
narrative_ontology:cs_axiom('fd0a5531-528d-43d1-8f4b-6ef3710765d5', secondary, teacher_professional_judgment_supersedes_scripted_programs).
narrative_ontology:cs_axiom_status(teacher_professional_judgment_supersedes_scripted_programs, holdable).
narrative_ontology:cs_axiom_grounding('fd0a5531-528d-43d1-8f4b-6ef3710765d5', teacher_professional_judgment_supersedes_scripted_programs, conventional).
narrative_ontology:cs_reference_frame('fd0a5531-528d-43d1-8f4b-6ef3710765d5', progressive_literacy_tradition).
narrative_ontology:cs_drift_state('fd0a5531-528d-43d1-8f4b-6ef3710765d5', post_national_reading_panel_2000, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fd0a5531-528d-43d1-8f4b-6ef3710765d5', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, literacy_coaches_whole_language).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teacher_education_faculty_wl).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_low_print_home).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_dyslexia_undiagnosed).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_english_learners).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, reading_as_meaning_construction).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, teacher_professional_judgment_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teachers trained in whole language pedagogy build professional identity around responsive, literature-rich instruction and rejection of scripted phonics programs. Their professional judgment is the primary instructional authority; they experience the constraint as preserving their autonomy against external mandates. Exit means abandoning a core professional self-concept and community.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_teachers, beneficiary,
    organized, biographical, identity_locked, national).

% District-level coaches and curriculum leaders who set the instructional vision, select materials, and evaluate teacher practice through a whole language lens. They administer the constraint by controlling professional development, assessment frameworks, and hiring priorities. Can move between districts or consultant roles if the paradigm shifts.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, literacy_coaches_whole_language, agenda_setter,
    institutional, generational, arbitrage, national).

% University faculty who prepare preservice teachers in whole language frameworks. They control the pipeline of new teachers and the research agenda that legitimizes the approach. Tenure and institutional reputation bind them; exit requires rebuilding a career research program.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teacher_education_faculty_wl, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, teacher_education_faculty_wl, beneficiary).

% Children from homes with limited books, low adult literacy, or non-English home language who enter school without the print exposure whole language assumes. They receive no systematic decoding instruction and fall behind in foundational skills. Cannot exit the classroom; families lack resources for tutoring or school choice.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_low_print_home, payer,
    powerless, biographical, trapped, local).

% Students with dyslexia or other decoding-related learning differences who require explicit, systematic phonics instruction to develop reading fluency. Whole language's rejection of direct decoding instruction leaves them without the specific instruction they need. Diagnosis often delayed because the framework attributes struggles to 'not enough engagement' rather than instructional mismatch.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_dyslexia_undiagnosed, payer,
    powerless, biographical, trapped, local).

% Students learning English as an additional language who need explicit instruction in English phoneme-grapheme correspondences that differ from their home language. Whole language's assumption that phonics emerges naturally through exposure fails when the sound-symbol system is unfamiliar. Some bilingual programs provide structured literacy; most mainstream whole language classrooms do not.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_english_learners, payer,
    powerless, biographical, constrained, local).

% Researchers in cognitive psychology, neuroscience, and linguistics who study reading acquisition experimentally. They observe the constraint from outside the classroom, producing evidence on decoding's necessity and whole language's differential effects. Their exit is analytical — they evaluate the constraint but are not subject to it.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, reading_scientists_cognitive_psychologists, observer,
    analytical, civilizational, analytical, global).

% Parent-led advocacy organizations (e.g., Decoding Dyslexia, The Reading League) demanding evidence-based structured literacy instruction. They are excluded from curriculum adoption decisions dominated by whole language institutional networks. They organize politically, lobby for legislation, and pursue due process complaints — constrained by institutional inertia and professional gatekeeping.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_advocates_science_of_reading, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared pedagogical framework that coordinates teacher practice around literature-rich environments, student choice, and meaning-centered instruction — replacing fragmented basal reader programs with a coherent philosophy of literacy as constructive meaning-making.
% TRANSFER_FUNCTION: Transfers instructional authority and professional status to teachers who embody the whole language stance; transfers the cost of reading failure to students who lack the home print environment the theory assumes. Time, confidence, and academic trajectory flow from vulnerable students to the professional community that maintains the framework.
% ABSENT_VOICES: Students who cannot yet articulate what instruction they need; families without literacy capital or English fluency to navigate school systems; cognitive scientists whose evidence on decoding necessity is dismissed as 'reductionist' or 'not classroom-realistic' within whole language professional communities.
% DISAPPEARANCE_RATIONALE: If whole language ideology vanished overnight, teacher preparation programs would restructure around evidence-based decoding instruction; curriculum adoption would shift to structured literacy materials; students currently receiving no systematic phonics would get explicit decoding instruction; the professional identity of a generation of teachers would be destabilized; advocacy organizations would declare victory and redirect energy.
% FOUNDING_PROBLEM: Mid-20th century reading instruction relied on rigid basal readers, isolated skill drills, and ability grouping that tracked low-income children into dead-end instructional paths. Whole language emerged to restore meaning, literature, and teacher agency to reading pedagogy.
% FOUNDING_PROBLEM_CORROBORATION: Historians of reading education (e.g., Diane Ravitch, Richard Allington) document the basal reader era's flaws. Cognitive scientists (e.g., Keith Stanovich, Linnea Ehri) and dyslexia researchers (e.g., Sally Shaywitz) attest that the founding problem's solution created a new problem: rejecting explicit decoding instruction harmed the very children progressive educators sought to help. The corroboration comes from outside the beneficiary set — from researchers and advocates the whole language establishment opposed.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the reading failure rate among students without home print support under whole language — the constraint extracts their academic trajectory to sustain the professional framework. Suppression (0.42) is moderate: whole language does not legally ban phonics, but it controls teacher preparation, professional development, and curriculum adoption to make systematic phonics professionally risky and practically unavailable. Theater ratio (0.31) is significant: the 'literature-rich environment' and 'teacher as decision-maker' rhetoric performs coordination while the actual mechanism sorts children by home background. Accessibility collapse (0.48) is partial — alternatives (structured literacy) exist and are known, but institutional barriers prevent adoption. Resistance (0.55) is substantial from science-of-reading advocates, parents, and legislators — but the constraint persists through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the teacher/coach seat, the constraint is a rope — it coordinates meaningful practice and protects professional judgment. From the vulnerable student seat, it is a snare — it withholds necessary instruction and suppresses alternatives. The engine computes this divergence from the declared structural roles and exit options; the authored claim (tangled_rope) acknowledges both functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language teachers are beneficiaries (d ~ 0.2) — the constraint validates their professional identity and autonomy. Literacy coaches and teacher education faculty are agenda-setters with arbitrage/constrained exit — they administer the constraint and benefit from its institutional embeddedness. Students lacking home print, undiagnosed dyslexia, or English proficiency are payers (d ~ 0.9) — they bear the cost of the constraint's false assumption about natural emergence. Parents and cognitive scientists are excluded/observers — they see the structure but cannot change it from their positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (basal reader rigidity) was real but has been substantially solved by modern structured literacy programs that combine explicit decoding with rich literature. The constraint persists not because the original problem remains, but because the professional identity and institutional infrastructure built around whole language have become self-sustaining. Mandatrophy is unresolved — the arrangement has outlived its founding justification but resists revision through identity-locked professional communities and institutional gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergent_literacy_assumption_validity,
    'Does phonological awareness and decoding skill actually emerge naturally from text exposure for all children, or only for those with sufficient home print experience and no processing differences?',
    'Longitudinal studies tracking decoding growth in whole language vs. structured literacy classrooms, disaggregated by home literacy environment and neurocognitive profile.',
    'If emergence is conditional on home advantage and neurotypical processing, the constraint''s coordination function is parasitic on inequality — extraction is structural, not incidental. If emergence is universal, the constraint is a genuine rope with incidental implementation gaps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergent_literacy_assumption_validity, empirical, 'Whether the core theoretical claim (natural emergence) holds universally or only for privileged subgroups.').

omega_variable(
    professional_identity_vs_student_outcomes,
    'Is the constraint''s persistence driven primarily by genuine belief in its pedagogical efficacy, or by the professional identity and institutional capital invested in it?',
    'Analysis of teacher preparation curriculum change resistance when faced with contrary evidence; tracking of faculty hiring, publication venues, and accreditation standards in literacy education.',
    'If identity/investment drives persistence more than evidence, the constraint is a piton or snare masquerading as a rope. If belief is evidence-responsive, the constraint may shift toward balanced/structured literacy as evidence accumulates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(professional_identity_vs_student_outcomes, conceptual, 'Whether the constraint''s maintenance is epistemically or identity-motivated.').

omega_variable(
    balanced_literacy_as_absorption_mechanism,
    'Does ''balanced literacy'' function as a genuine synthesis or as an absorption mechanism that preserves whole language''s core rejection of systematic phonics while adopting its terminology?',
    'Classroom observation studies measuring actual instructional time allocation and instructional sequence in self-identified balanced literacy classrooms.',
    'If absorption, the constraint family maintains extraction through rebranding; the kernel''s contested structure is preserved rather than resolved. If genuine synthesis, the constraint family is converging toward a lower-extraction equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balanced_literacy_as_absorption_mechanism, empirical, 'Whether balanced literacy is a true integration or a protective rebranding of whole language.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of systematic phonics instruction structural (institutional gatekeeping, accreditation, curriculum control) or internalized (teachers genuinely believe phonics harms children)?',
    'Survey and interview studies of teachers in whole language contexts: do they want to teach phonics but feel unable, or do they believe it is pedagogically wrong?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — teachers carry the suppression with them even if institutional barriers fall. If structural, removal of gatekeeping may rapidly shift practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in teacher practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 1975, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1975, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(lite_tr_t1985, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(lite_tr_t1995, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(lite_be_t1975, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(lite_be_t1985, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(lite_be_t1995, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1975, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1975, 0.2).
narrative_ontology:measurement(lite_su_t1985, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement(lite_su_t1995, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__whole_language_reading, 0.08).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, teacher_preparation_accreditation).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, state_reading_curriculum_mandates).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel decomposes into four constraint stories (whole_language, phonics, balanced_literacy, structured_literacy) linked by affects_constraints. Each reading instantiates a different constraint with distinct extractiveness profiles: whole_language (this story) extracts from print-poor students; phonics_reading extracts from teacher autonomy; structured_literacy_reading extracts from instructional flexibility but distributes gains more evenly; balanced_literacy_reading claims synthesis but may function as whole_language absorption. The ε values differ because the referent (the standing arrangement under contest) is evaluated from each reading's structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__whole_language_reading, organized, 0.15).
constraint_indexing:directionality_override(literacy_acquisition_kernel__whole_language_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
