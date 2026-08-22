% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__whole_language_meaning_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole Language Meaning-Primacy Legitimacy Constraint
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint story models the whole-language meaning-primacy reading
 *   of the reading_acquisition_legitimacy kernel. It asserts that reading is
 *   fundamentally meaning-making and that legitimate instruction immerses
 *   children in authentic literature from day one, allowing decoding to
 *   emerge naturally through exposure and context. The constraint coordinates
 *   teacher practice around low-structure immersion, authentic texts, running
 *   records, and guided reading. It extracts from children who need explicit
 *   systematic decoding (especially those with dyslexia or from low-SES
 *   backgrounds without home literacy scaffolding), from early-career
 *   teachers denied phonics knowledge in preparation programs, and from
 *   public funding streams that sustain ineffective interventions. The
 *   constraint persists through active enforcement: teacher education
 *   programs that omit or marginalize systematic phonics, curriculum
 *   adoptions that privilege leveled readers over decodable texts,
 *   professional development that frames explicit instruction as harmful, and
 *   institutional gatekeeping that treats phonics advocacy as ideologically
 *   suspect. The claimed type is tangled_rope because there IS a genuine
 *   coordination function — creating print-rich environments, centering
 *   comprehension, valuing children's meaning-making — but this coordination
 *   is fused with asymmetric extraction that harms identifiable victims, and
 *   the arrangement requires active enforcement to suppress structured
 *   alternatives.
 *
 * KEY AGENTS:
 *   - whole_language_practitioners: Primary beneficiaries (moderate/identity_locked) — professional identity fused with meaning-primacy; exit threatens self-concept
 *   - progressive_education_establishment: Beneficiaries (institutional/arbitrage) — institutional authority, grant streams, publication venues
 *   - teacher_education_faculty_whole_language: Beneficiaries/agenda_setters (institutional/identity_locked) — control preparation curricula, gatekeep certification
 *   - balanced_literacy_publishers_heinemann: Beneficiaries (powerful/arbitrage) — commercial curriculum empire built on the constraint
 *   - children_with_dyslexia: Primary victims (powerless/trapped) — biological need for explicit decoding denied; no exit from public schooling
 *   - struggling_readers_low_ses: Victims (powerless/trapped) — lack home literacy capital to compensate for school's instructional gaps
 *   - early_career_teachers_without_phonics_knowledge: Victims (moderate/constrained) — enter classrooms unprepared; exit requires costly self-directed learning
 *   - taxpayers_funding_ineffective_interventions: Victims (organized/mobile) — diffuse bearers of opportunity cost; exit via political pressure
 *   - reading_scientists_cognitive_psychologists: Excluded/observers (analytical/analytical) — empirical evidence systematically marginalized in teacher education
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.68).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.62).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Meaning-Primacy Legitimacy Constraint").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'c177b495-8548-4e95-8a23-3a44333badcc').
narrative_ontology:cs_kernel_codification('c177b495-8548-4e95-8a23-3a44333badcc', distributed).
narrative_ontology:cs_authority_grounding('c177b495-8548-4e95-8a23-3a44333badcc', practice).
narrative_ontology:cs_interpretation_layer_present('c177b495-8548-4e95-8a23-3a44333badcc').
narrative_ontology:cs_reading_relation('c177b495-8548-4e95-8a23-3a44333badcc', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('c177b495-8548-4e95-8a23-3a44333badcc', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('c177b495-8548-4e95-8a23-3a44333badcc', reading_acquisition_legitimacy__structured_literacy_remediation, forecloses).
narrative_ontology:cs_axiom('c177b495-8548-4e95-8a23-3a44333badcc', foundational, meaning_making_is_reading).
narrative_ontology:cs_axiom_status(meaning_making_is_reading, holdable).
narrative_ontology:cs_axiom_grounding('c177b495-8548-4e95-8a23-3a44333badcc', meaning_making_is_reading, deontological).
narrative_ontology:cs_axiom('c177b495-8548-4e95-8a23-3a44333badcc', foundational, decoding_emerges_naturally_from_meaning_immersion).
narrative_ontology:cs_axiom_status(decoding_emerges_naturally_from_meaning_immersion, overridden).
narrative_ontology:cs_axiom_grounding('c177b495-8548-4e95-8a23-3a44333badcc', decoding_emerges_naturally_from_meaning_immersion, empirically_contingent).
narrative_ontology:cs_axiom('c177b495-8548-4e95-8a23-3a44333badcc', secondary, authentic_texts_from_day_one).
narrative_ontology:cs_axiom_status(authentic_texts_from_day_one, holdable).
narrative_ontology:cs_axiom_grounding('c177b495-8548-4e95-8a23-3a44333badcc', authentic_texts_from_day_one, deontological).
narrative_ontology:cs_axiom('c177b495-8548-4e95-8a23-3a44333badcc', secondary, teacher_as_facilitator_not_technician).
narrative_ontology:cs_axiom_status(teacher_as_facilitator_not_technician, holdable).
narrative_ontology:cs_axiom_grounding('c177b495-8548-4e95-8a23-3a44333badcc', teacher_as_facilitator_not_technician, deontological).
narrative_ontology:cs_reference_frame('c177b495-8548-4e95-8a23-3a44333badcc', progressive_literacy_tradition).
narrative_ontology:cs_drift_state('c177b495-8548-4e95-8a23-3a44333badcc', post_national_reading_panel_2000, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c177b495-8548-4e95-8a23-3a44333badcc', '2026-08-10T14:32:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_practitioners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, progressive_education_establishment).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_education_faculty_whole_language).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, balanced_literacy_publishers_heinemann).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_with_dyslexia).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers_low_ses).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, early_career_teachers_without_phonics_knowledge).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, taxpayers_funding_ineffective_interventions).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, meaning_making_is_reading).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, authentic_texts_engage_children).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_as_facilitator_not_technician).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classroom teachers whose professional identity, community, and pedagogical repertoire are constituted through whole-language/balanced literacy frameworks. They genuinely believe in meaning-primacy, use running records and guided reading daily, and experience explicit phonics advocacy as an attack on their professionalism. Exit requires abandoning their self-concept as 'teachers who teach reading through meaning' — not just learning new skills but becoming a different kind of teacher.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_practitioners, beneficiary,
    moderate, biographical, identity_locked, national).

% Education school deans, curriculum theorists, professional organization leadership (IRA/ILA, NCTE), and foundation program officers who control grant streams, conference agendas, journal editorships, and accreditation influence. They benefit from the constraint's legitimation of progressive pedagogy as the humane, child-centered alternative to 'drill and kill.' They can arbitrage across institutions — moving between universities, foundations, and NGOs — and their exit options are wide.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, progressive_education_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, progressive_education_establishment, agenda_setter).

% Literacy faculty in colleges of education who design and teach reading methods courses. They control what 200,000+ teacher candidates per year learn about reading instruction. Their expertise, publications, and professional standing are built on whole-language/balanced literacy frameworks. Admitting the scientific consensus on explicit decoding would invalidate their life's work. They are identity-locked: exit means acknowledging their preparation programs have been miseducating teachers for decades.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_education_faculty_whole_language, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_education_faculty_whole_language, beneficiary).

% Commercial publishers (Heinemann, Fountas & Pinnell, Lucy Calkins/TCRWP) whose curriculum products — Units of Study, Leveled Literacy Intervention, running record assessments — generate hundreds of millions in revenue. The constraint creates the market for their products: leveled readers replace decodables, running records replace phonics assessments, guided reading replaces explicit instruction. They have arbitrage-grade exit: they could pivot to structured literacy products (some are beginning to), but the constraint's persistence protects their current revenue model.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, balanced_literacy_publishers_heinemann, beneficiary,
    powerful, biographical, arbitrage, national).

% Children with a neurobiological difference requiring explicit, systematic, cumulative decoding instruction. In whole-language/balanced literacy classrooms, they receive neither the intensity nor the structure they need. They are trapped: compulsory attendance laws, lack of private alternatives for most families, and teacher unpreparedness mean they cannot access appropriate instruction within the public system. The constraint extracts years of academic failure, secondary behavioral and emotional harm, and lifelong opportunity cost.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_with_dyslexia, payer,
    powerless, biographical, trapped, local).

% Children from low-SES backgrounds who enter school with less oral language exposure, fewer books at home, and less explicit print awareness. They depend entirely on school for systematic reading instruction. The constraint's meaning-primacy immersion assumes home literacy capital that these children lack. They are trapped in the same structural way as children with dyslexia but without the legal protections of IDEA. The constraint extracts the Matthew effect: early gaps widen because the instruction doesn't close them.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers_low_ses, payer,
    powerless, biographical, trapped, local).

% New teachers who graduate from whole-language-dominated preparation programs knowing how to administer running records and conduct guided reading but not how to teach phonemic awareness, sound-spelling correspondences, or decoding strategies. They bear the cost of the constraint's extraction daily: students who don't learn to read, frustration, and the burden of self-directed remediation (LETRS, Orton-Gillingham training) at personal expense. Exit is constrained: they can learn structured literacy, but it takes 100+ hours and $2,000-5,000 of personal investment.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, early_career_teachers_without_phonics_knowledge, payer,
    moderate, biographical, constrained, local).

% The public bears the cost of reading failure: special education placements (many preventable with early explicit instruction), remedial programs, grade retention, dropout consequences, and the curriculum/materials purchases that sustain the constraint. As an organized polity (school boards, state legislatures, voters), they have mobile exit: they can elect officials who mandate structured literacy, adopt new standards, and defund ineffective programs. This exit is being exercised in 40+ states with 'science of reading' legislation since 2019.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, taxpayers_funding_ineffective_interventions, payer,
    organized, biographical, mobile, national).

% Researchers in cognitive psychology, neuroscience, and linguistics who study reading development (e.g., Stanislas Dehaene, Keith Stanovich, Linnea Ehri, David Share). Their converging evidence — the Simple View of Reading, orthographic mapping, the self-teaching hypothesis — establishes that explicit decoding is necessary for most and critical for vulnerable learners. They are excluded from teacher education curricula, professional development, and curriculum adoption committees. Their exclusion is not incidental: it is the enforcement mechanism that maintains the constraint's suppression.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_scientists_cognitive_psychologists, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates print-rich, meaning-centered classroom environments where children engage with authentic literature daily, develop comprehension strategies, and experience reading as a meaningful activity rather than a decoding exercise. Solves the problem of rigid, disconnected phonics drills that killed motivation and ignored comprehension.
% TRANSFER_FUNCTION: Moves instructional time, teacher attention, curriculum budgets, and professional development resources away from explicit systematic decoding instruction toward meaning-primacy routines (read-alouds, shared reading, guided reading, running records). Moves the cost of reading failure onto vulnerable children and their families. Moves commercial revenue to balanced literacy publishers.
% ABSENT_VOICES: Children with dyslexia and their parents — they would object to the denial of explicit instruction but are not in curriculum adoption rooms. Reading scientists — their evidence is systematically excluded from teacher preparation and professional development. Early-career teachers — they would object to being sent into classrooms unprepared but have no voice in preparation program design.
% DISAPPEARANCE_RATIONALE: If the whole-language meaning-primacy constraint vanished overnight, teacher preparation programs would rapidly incorporate explicit systematic phonics (many already are under legislative pressure). Curriculum adoptions would shift from leveled readers to decodable texts. Professional development would pivot from running records to phonics assessments and structured literacy routines. Children with dyslexia and low-SES struggling readers would receive the explicit instruction they need. The balanced literacy publisher empire would face existential disruption. The mobile exit of taxpayers/legislatures is already driving this rearrangement.
% FOUNDING_PROBLEM: Mid-20th century reading instruction was dominated by basal readers with rigid, isolated phonics drills, controlled vocabulary, and minimal authentic literature. Children were bored, comprehension was neglected, and reading was experienced as a school task rather than a meaning-making activity. The whole-language movement emerged in the 1970s-80s as a legitimate corrective: immerse children in real books, center meaning, trust the child's language learning capacity.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (rigid drill-heavy basals neglecting meaning) is dead: no current curriculum resembles 1970s basals. Even phonics_primacy advocates (e.g., National Reading Panel, 2000) emphasize that phonics must be embedded in rich literacy environments with abundant reading of authentic texts. The corroboration comes from outside the beneficiary set: the National Reading Panel (congressionally mandated, scientist-led), the What Works Clearinghouse practice guides, 40+ state 'science of reading' laws passed 2019-2024, and the International Dyslexia Association — none of which are whole-language beneficiaries. The whole-language establishment (ILA, NCTE, teacher education faculty) disputes this status, claiming the problem persists in 'phonics-first' approaches — but this is a cover-story for the constraint's persistence.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the constraint's high cost to vulnerable children and the public purse relative to its coordination benefit. The coordination function (print-rich environments, comprehension focus) is real but incomplete — it works for children with strong oral language and home literacy but fails the 20-30% who need explicit decoding. Suppression (0.62) is substantial: the constraint survives by marginalizing phonics in teacher preparation, controlling curriculum adoption committees, framing explicit instruction as 'drill and kill,' and treating reading scientists as ideologically motivated. Theater ratio (0.48) is elevated: running records and guided reading create an appearance of individualized assessment and instruction, but the assessment tools (MSV cues) and instructional routines (three-cueing) are misaligned with how reading actually develops, generating performative data that masks the absence of systematic decoding instruction. Accessibility collapse (0.45) is moderate: alternatives (structured literacy, explicit phonics) exist and are empirically validated, but institutional barriers make them inaccessible to most teachers and students. Resistance (0.71) is high: reading scientists, dyslexia advocates, parents, and increasingly state legislatures actively contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the whole-language practitioner seat (identity_locked, moderate power), the constraint appears as genuine coordination — a humane, child-centered approach that respects meaning-making. The extraction is invisible because the practitioner's professional identity is constituted through the constraint. From the child-with-dyslexia seat (trapped, powerless), the same constraint is a snare: it denies the explicit instruction their neurobiology requires, and exit is structurally blocked by compulsory schooling and teacher unpreparedness. From the teacher-education-faculty seat (institutional, identity_locked), the constraint is both coordination (a shared pedagogical framework) and extraction (protecting faculty expertise and curriculum control from scientific revision). The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: whole_language_practitioners (professional identity, publication, community), progressive_education_establishment (institutional authority, grant control), teacher_education_faculty_whole_language (curriculum control, gatekeeping), balanced_literacy_publishers_heinemann (commercial curriculum revenue). These groups sit at the beneficiary end of directionality (low d) — the constraint subsidizes their professional standing, revenue, and authority. Victims declared: children_with_dyslexia (biologically require explicit decoding, trapped in system), struggling_readers_low_ses (lack compensatory home resources, trapped), early_career_teachers_without_phonics_knowledge (enter profession unprepared, constrained exit), taxpayers_funding_ineffective_interventions (diffuse cost bearers, mobile exit via politics). These sit at the target end (high d) — the constraint extracts from them. Reading scientists are excluded observers: they bear no direct extraction but their evidence is suppressed; they sit at analytical d=0.5. The directionality derivation chain reads these structural positions and computes effective extraction accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early reading instruction that was rigid, drill-heavy, and disconnected from meaning) was real and live in the 1970s-1980s. The constraint was built as a scaffold to solve it — but the scaffold never sunset. By the 2000s, cognitive science had established that explicit systematic decoding is necessary for most children and critical for vulnerable populations. The constraint's mandate (meaning-primacy immersion) outlived its function; it now persists through institutional inertia and identity fusion. The mandatrophy is unresolved: the arrangement continues to extract from children who need explicit instruction while the beneficiaries' professional identity prevents acknowledgment of the founding problem's resolution. The founding_problem_status is 'dead' (the rigid-drill problem is solved; the current problem is under-preparation in decoding), but the constraint persists as if the problem were still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the whole_language_meaning_primacy reading foreclose the phonics_decoding_primacy reading within a single instructional framework, or do they coexist as competing frameworks adopted by different parties?',
    'Analyze whether a single teacher/school/district can simultaneously hold meaning-primacy and decoding-primacy as core instructional logics. Empirical evidence: balanced literacy attempted synthesis but in practice defaulted to meaning-primacy with phonics as decorative add-on.',
    'If forecloses, the kernel has mutually exclusive readings — adoption of one logically excludes the other. If coexists_with, the constraint family maps a genuine policy dispute with multiple live positions. If influences, the whole-language reading''s institutional dominance creates structural pressure on phonics adoption (resource allocation, teacher preparation) without logical foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between whole-language and phonics-primacy readings of the same kernel').

omega_variable(
    balanced_literacy_as_influenced_synthesis,
    'Is balanced_literacy_integration a genuine coexistence of both readings, or does it structurally inherit the whole-language reading''s extraction while borrowing phonics vocabulary?',
    'Compare balanced literacy implementations: do they deliver systematic explicit phonics (phonics_primacy''s core) or incidental analytic phonics embedded in meaning-primacy routines? Curriculum audits and classroom observation studies.',
    'If balanced literacy is whole-language-influenced rather than genuine coexistence, the kernel''s constraint family has two extraction-heavy readings (whole_language and balanced_literacy) and one coordination-heavy reading (phonics_primacy) — changing the network dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balanced_literacy_as_influenced_synthesis, empirical, 'Whether balanced literacy is a distinct reading or whole-language''s adaptive colonization of phonics terminology').

omega_variable(
    teacher_identity_fusion_mechanism,
    'What specific identity-fusion mechanism binds whole-language practitioners to the constraint — professional identity (career path dependence), ideological identity (progressive education worldview), relational identity (community of practice), or institutional identity (the program has ''become'' its function)?',
    'Longitudinal studies of teacher belief change when exposed to reading science; analysis of professional development resistance patterns; qualitative interviews with teachers who shifted from whole-language to structured literacy.',
    'If identity fusion is primarily ideological, the constraint''s persistence is belief-driven and may shift with worldview change. If professional/institutional, persistence is structural and requires institutional reform. The exit_options for practitioners (currently identity_locked) would reclassify if the mechanism breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_identity_fusion_mechanism, conceptual, 'Identity-lock mechanism for whole-language practitioners').

omega_variable(
    suppression_mechanism_teacher_education,
    'Is the suppression of phonics in teacher education structural (accreditation standards, faculty hiring, curriculum mandates) or internalized (faculty genuinely believe phonics is harmful, graduates internalize this belief)?',
    'Post-exit suppression trajectory: track teachers who receive structured literacy training after certification — do they adopt it readily (suppression was structural) or resist it (suppression was internalized)?',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the teacher into new contexts. If structural, removing the structural barrier (accreditation reform) would rapidly change practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_teacher_education, empirical, 'Structural vs. internalized suppression in teacher preparation').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (print-rich environments, comprehension focus, child agency) structurally separable from the extraction function (denying explicit decoding to children who need it, suppressing phonics in teacher prep)?',
    'Natural experiment: schools/districts that adopt structured literacy (explicit decoding + rich literature + comprehension) — do they retain the coordination benefits while eliminating the extraction? Compare student outcomes, teacher satisfaction, and equity metrics.',
    'If separable, the constraint is a true tangled_rope: genuine coordination fused with removable extraction. If inseparable, the coordination story may be cover for extraction (snare-like) or the extraction may be the price of the coordination (tangled_rope with high necessary extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 1985, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_tr_t1985, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_tr_t1995, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_tr_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_tr_t2005, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2005, 0.52).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_tr_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_tr_t2015, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2015, 0.46).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_tr_t2019, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2019, 0.44).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_tr_t2024, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_be_t1985, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_be_t1995, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_be_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_be_t2005, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_be_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_be_t2015, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_be_t2019, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_be_t2024, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_su_t1985, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_su_t1995, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_su_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_su_t2005, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_su_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_su_t2015, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_su_t2019, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(reading_acquisition_legitimacy__whole_language_meaning_primacy_su_t2024, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reading_acquisition_legitimacy kernel. The kernel decomposes into four constraint stories with different ε values and beneficiary/victim structures: whole_language_meaning_primacy (this story, ε=0.68, tangled_rope), balanced_literacy_integration (ε≈0.55, tangled_rope), phonics_decoding_primacy (ε≈0.25, rope), structured_literacy_remediation (ε≈0.15, rope/scaffold). The whole-language reading influenced the balanced literacy reading's development (influences relation); both meaning-primacy readings foreclose the decoding-primacy reading in a single instructional framework (forecloses relation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, moderate, 0.75).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
