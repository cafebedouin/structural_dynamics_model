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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition (Meaning-First Pedagogy)
 *   domain: educational/literacy/cognitive-science
 *
 * SUMMARY:
 *   This is ONE READING of the contested literacy acquisition kernel. The
 *   whole language reading instantiates a pedagogical commitment that reading
 *   acquisition emerges naturally from meaningful engagement with connected
 *   text; phonics skills develop incidentally through exposure and context;
 *   explicit decoding instruction is unnecessary and potentially harmful to
 *   reading motivation. This reading is held by a coalition of progressive
 *   educators, teacher-training institutions, and administrators committed to
 *   constructivist epistemology. The reading vindicates teacher professional
 *   judgment and child-centered pedagogy. Structurally, it benefits teachers
 *   (preserved autonomy), print-rich students (for whom contextual learning
 *   succeeds), and the educational establishment that theorized it. It
 *   extracts from students without home literacy backgrounds (who lack the
 *   assumed scaffolding) and from students with phonological processing
 *   deficits (for whom pattern induction from connected text is
 *   systematically ineffective). The measurement trajectory shows
 *   extractiveness and theater ratio rising modestly over the 40-year
 *   interval (0.52→0.68 extractiveness; 0.28→0.41 theater ratio), indicating
 *   increasing layering of theoretical justification onto a core function
 *   that is increasingly observed to advantage print-rich students.
 *   Suppression requirement also rises (0.38→0.52), documenting the
 *   institutional effort required to exclude contradictory evidence and
 *   maintain the reading against mounting cognitive science findings.
 *
 * KEY AGENTS:
 *   - teachers_professional_autonomy — organized beneficiary, preserved through constrained exit, national scope
 *   - print_rich_home_students — powerless beneficiary, trapped by home advantage, local scope
 *   - students_without_print_rich_homes — powerless victim, trapped by lack of home literacy, local scope
 *   - students_with_dyslexia_or_phonological_processing_deficits — powerless victim, trapped by pedagogical mismatch, local scope
 *   - phonics_reading_advocates — excluded from curriculum policy, mobile but organizationally excluded
 *   - school_administrators — institutional agenda-setter enforcing the constraint through material and hiring decisions
 *   - curriculum_publishers — institutional beneficiary via adoption of connected-text materials
 *   - literacy_education_establishment — non-agent beneficiary (doctrinal vindication)
 *   - observer_reading_science — analytical observer documenting divergence between reading science and pedagogical practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.52).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Acquisition (Meaning-First Pedagogy)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational/literacy/cognitive-science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '17e9f4f1-4ec1-4373-a89b-2e864bf2774a').
narrative_ontology:cs_kernel_codification('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', distributed).
narrative_ontology:cs_authority_grounding('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', extraction).
narrative_ontology:cs_interpretation_layer_present('17e9f4f1-4ec1-4373-a89b-2e864bf2774a').
narrative_ontology:cs_reading_relation('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', foundational, reading_is_natural_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_natural_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', reading_is_natural_meaning_making, deontological).
narrative_ontology:cs_axiom('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', secondary, explicit_decoding_instruction_is_contraindicated).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_is_contraindicated, overridden).
narrative_ontology:cs_axiom_grounding('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', explicit_decoding_instruction_is_contraindicated, empirically_contingent).
narrative_ontology:cs_reference_frame('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', child_centered_natural_acquisition_framework).
narrative_ontology:cs_drift_state('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', contemporary_cognitive_science_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('17e9f4f1-4ec1-4373-a89b-2e864bf2774a', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teachers_professional_autonomy).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, print_rich_home_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_print_rich_homes).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia_or_phonological_processing_deficits).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, parents_affluent).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, parents_low_income).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teachers adopting whole language are positioned as professional practitioners exercising responsive judgment about children's learning. The pedagogy vindicates intuitive understanding of child development and rejects external scripting. They retain autonomy in lesson design, pacing, and material selection. Professional identity is invested in meaning-centered, child-responsive instruction.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teachers_professional_autonomy, beneficiary,
    organized, biographical, constrained, national).

% Children from homes with abundant books, literate parents, and language-rich conversation arrive at school with phonological awareness, letter familiarity, and narrative comprehension already developed. Whole language instruction leverages this prior knowledge. They thrive through engagement with literature and naturally develop decoding skills from contextual exposure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, print_rich_home_students, beneficiary,
    powerless, biographical, trapped, local).

% Children without home print exposure, limited parental literacy, and sparse prior language experience face a systematic disadvantage. Whole language instruction assumes the contextual and phonological scaffolding they lack. They are expected to infer phoneme-grapheme patterns from connected text they cannot yet decode. They have no way to exit (trapped) and bear cumulative reading failure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_print_rich_homes, payer,
    powerless, biographical, trapped, local).

% Students with phonological processing deficits, dyslexia, or difficulty extracting phoneme-level patterns cannot infer decoding from context and require explicit, systematic instruction in letter-sound relationships. Whole language pedagogy, which relies on pattern induction and contextual inference, is systematically ineffective. They remain non-fluent readers and accumulate academic and social consequences.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia_or_phonological_processing_deficits, payer,
    powerless, biographical, trapped, local).

% Cognitive scientists, special educators, and systematic phonics advocates produce reading science evidence and advocate for explicit decoding instruction. They are organizationally excluded from curriculum policy spaces dominated by whole language constituencies. Their findings are marginalized, reframed as 'reductive,' or treated as incompatible with child-centered ideology.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, phonics_reading_advocates, excluded,
    organized, generational, mobile, national).

% District reading coordinators and principals committed to whole language set curriculum policy, allocate professional development funding, hire teachers trained in meaning-centered methods, and resist adoption of phonics materials. They enforce gatekeeping against alternative readings through material selection and hiring.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, school_administrators, agenda_setter,
    institutional, generational, constrained, regional).

% Publishing houses producing basal readers and trade books benefit from whole language adoption. Material adoption contracts drive demand for connected-text collections, reader response guides, and literature circles resources. They have institutional incentive to support the reading's continued dominance.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Schools of education, professional organizations (International Reading Association, National Council of Teachers of English), and literacy journals have built theoretical frameworks, hiring, curriculum, and publication records around constructivist epistemology and whole language pedagogy. The constraint vindicates institutional commitments and disciplinary identity.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, literacy_education_establishment, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(literacy_acquisition_kernel__whole_language_reading, literacy_education_establishment).

% Affluent parents whose children succeed in whole language settings often endorse the child-centered, meaning-focused ideology. Their children's reading success under the approach confirms it. They have exit options (private schools, supplemental tutoring, summer reading camps) if problems emerge, so they face low cost for endorsing the reading.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_affluent, beneficiary,
    powerful, biographical, mobile, local).

% Low-income parents lack resources for supplemental tutoring, private school alternatives, or building extensive home libraries. If their child is failing to develop reading fluency under whole language instruction, they have constrained exit options. They bear the cost of a pedagogical approach predicated on home literacy resources they cannot provide.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_low_income, payer,
    moderate, biographical, constrained, local).

% Cognitive scientists, psycholinguists, neuroscientists, and reading researchers study the mechanisms of orthographic learning and phonological processing. They document through empirical methods that reading requires explicit phoneme awareness and decoding instruction plus meaningful engagement. They observe the constraint's institutional persistence despite contradictory evidence.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, observer_reading_science, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, literacy_education_establishment).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom instruction around principles of child-centered learning, response to student interest, and engagement with authentic literature. Solves the coordination problem of aligning instructional design with children's motivation and meaning-making without relying on external scripting or deskilling teachers to technique executors.
% TRANSFER_FUNCTION: Transfers instructional authority from centrally designed phonics sequences to individual teachers' judgment and professional autonomy. Transfers the assumption of phonological scaffolding from explicit school instruction to home literacy exposure. In aggregate, transfers reading development success burden from school provision toward students' pre-existing home advantages.
% ABSENT_VOICES: Cognitive scientists and special education researchers whose evidence contradicts the reading are organizationally absent from curriculum committees and teacher-training decisions. Parents of students with dyslexia and low-income parents whose children fail to develop fluency under the approach lack sufficient organizational power to reshape instruction. Phonics advocates are systematically excluded from policy spaces.
% DISAPPEARANCE_RATIONALE: If whole language pedagogy vanished from institutional implementation, school reading instruction would require substantial reorganization: curriculum would shift toward systematic phonics integrated with meaning-centered engagement (balanced or structured literacy), teacher training would emphasize phonological awareness and decoding scope-and-sequence, publishing would pivot to decodable readers alongside trade books, and students currently failing to develop fluency would receive explicit instruction. The institutional and material organization of reading instruction depends on this constraint's persistence.
% FOUNDING_PROBLEM: Mid-20th century reading instruction was mechanistic and decontextualized: scripted basal readers, isolated phonics drills, and workbook-driven teaching that disconnected decoding from meaning and pleasure. Children were trained to decode without engaging with literature as a social and intellectual practice.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates attest the founding problem of mechanistic, meaning-divorced instruction persists in phonics-heavy classrooms. Cognitive scientists and educators implementing balanced or structured literacy attest the founding problem is substantially solved: contemporary phonics instruction can be integrated with meaningful text engagement; the problem was not phonics per se but phonics divorced from comprehension. Special educators and parents of struggling readers attest the real problem is insufficient explicit phonemic instruction regardless of meaning engagement. Reading science evidence from the National Reading Panel (2000) and subsequent systematic reviews documents that the founding problem (over-mechanization, meaning-disconnection) has been addressed through approaches that combine systematic phonics with rich literature engagement.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-to-high (0.68) because the constraint distributes benefits and costs asymmetrically: teachers and affluent students benefit from meaning-centered, autonomous instruction; low-income and dyslexic students bear the cost of a pedagogical approach that assumes background knowledge they lack. The extractiveness is not so high as snare (0.68 < snare threshold ~0.75) because the constraint does deliver real coordination: it solves the genuine problem of child engagement with literature and teachers' autonomy—these are not pure cover stories. But the distribution of who benefits and who bears costs is structurally asymmetric. Suppression (0.52) reflects institutional effort to maintain the reading against contradictory evidence: curriculum committees resist phonics adoption, reading science findings are marginalized in teacher training, and schools establish gatekeeping that excludes phonics advocates from policy spaces. Theater ratio (0.41) indicates the constraint's function is increasingly performative: the pedagogical justification (child-centered, meaning-first) is authored with increasing theoretical elaboration (whole language theory, reader-response criticism, constructivism) as empirical evidence for the approach's universal effectiveness erodes. Accessibility collapse (0.47) is lower than mountains (~0.85) and moderate for ropes (~0.55): alternatives (phonics, balanced literacy, structured literacy) are intellectually accessible and increasingly advocated, but institutional barriers make them difficult to access within the adopting system. Resistance (0.73) is high because cognitive scientists, special educators, and parents of struggling readers actively oppose the reading, mounting evidence and policy advocacy against it.
 *
 * PERSPECTIVAL GAP:
 *   From the whole language teacher's seat: reading is naturally acquired through engagement with meaningful text; explicit phonics instruction is reductive, demotivating, and contradicts how children actually learn to read. From the low-income parent's seat: my child is failing to learn to read; the school refuses to teach phonics explicitly; I cannot afford private tutoring; my child is trapped in a system that assumes advantages we do not possess. From the cognitive scientist's seat: reading is a constructed skill requiring explicit decoding instruction; the whole language approach has been tested and found significantly less effective than systematic phonics or balanced approaches, particularly for students with processing deficits and low-literacy backgrounds; the constraint's persistence despite contradictory evidence indicates it is ideology rather than pedagogy. From the dyslexic student's seat: the method is not working for me; I cannot extract phonics patterns from connected text I cannot decode; I need explicit instruction in letter-sound relationships that the school refuses to provide. The engine computes these perspectival differences from the structural data: beneficiary seats compute to low d and low effective extraction; target seats compute to high d and high effective extraction; excluded seats report no voice in the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Teacher autonomy sits at d ≈ 0.1 (strong beneficiary): the whole language reading preserves professional judgment, expands their instructional authority, and vindicates their theoretical commitments. Print-rich students sit at d ≈ 0.15 (weak beneficiary): they benefit from engagement-centered instruction because their home background provides the missing scaffolding. Students without print-rich homes sit at d ≈ 0.85 (strong target): they are systematically disadvantaged by a pedagogy that assumes home literacy resources; they lack exit options (trapped power atom) and face cumulative reading failure. Students with dyslexia sit at d ≈ 0.90 (strongest target): the constraint is demonstrably ineffective for their neurological profile; phonological processing deficits make pattern induction from connected text a binding constraint; they have no exit within the system and high cost if they fail to develop fluency. School administrators and curriculum publishers sit at d ≈ 0.2 (beneficiaries): they control enforcement and accrue authority/market access from the reading's adoption. Phonics advocates sit at d ≈ 0.95 (fully excluded targets): they are excluded from policy spaces despite possessing superior evidence; their exclusion is structurally maintained by the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The whole language reading's founding problem—mechanical, decontextualized phonics instruction that alienates children from reading's social meaning—is substantially solved. Cognitive science has demonstrated that reading requires explicit phonological awareness and decoding instruction PLUS meaningful engagement; the two are not contradictory. Balanced and structured literacy approaches integrate both. Whole language persists as a constraint not because it solves the founding problem (balanced approaches do that better) but because it vindicates a particular set of professional commitments and theoretical frameworks. The constraint exhibits mandatrophy signals: (1) foundational dispute about whether the problem it addresses is live (education establishment says yes; reading science says substantially no); (2) misalignment between disappearance verdict (world_rearranges—children do need to learn to read, and instruction would shift) and founding problem status (dead—the problem is solved by alternatives); (3) rising theater ratio over time, indicating increasing theoretical elaboration as the core function's empirical support erodes. The constraint is maintained not by coordination failure (reading science has solved the problem) but by institutional capture: teacher-training programs, educational publishing, professional organizations, and curriculum policy are locked into the reading's theoretical framework and resist alternatives despite evidence. The extraction it produces (disadvantaging non-print-rich students and students with processing deficits) is structurally maintained by suppressing contradictory evidence and excluding phonics advocates from policy spaces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    home_literacy_assumption_necessity,
    'Is home print exposure an assumption whole language pedagogy necessarily carries, or can the approach scaffold non-readers to the same endpoint without assuming home literacy background?',
    'Longitudinal comparison of reading outcomes for print-rich vs. print-poor students under whole language instruction, controlling for teacher expertise and classroom text availability. If non-print-rich students reliably fail to develop fluency, the assumption is structural.',
    'If home literacy assumption is structural, whole language extracts from students lacking background knowledge, and should be classified as a tangled_rope (coordination for print-rich + extraction from print-poor) or snare (if the extraction is primary). If it is not structural, the constraint coordinates more broadly and extraction is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(home_literacy_assumption_necessity, empirical, 'Whether whole language pedagogy necessarily assumes home print exposure to function.').

omega_variable(
    phonological_processing_effectiveness_boundary,
    'For students with phonological processing deficits or dyslexia, is whole language pedagogy a mismatch of method and neurology, or can sufficient classroom scaffolding compensate?',
    'Randomized controlled trials comparing reading outcomes for students with documented phonological deficits under whole language vs. explicit phonics instruction, with equivalent classroom support in both arms.',
    'If whole language is systematically ineffective for phonologically vulnerable students, the constraint extracts from them structurally. If classroom support can compensate, extraction is less inevitable but depends on resource allocation and teacher expertise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonological_processing_effectiveness_boundary, empirical, 'Whether whole language is neuromatch for all learners or contraindicated for those with phonological processing deficits.').

omega_variable(
    reading_science_evidence_integration,
    'Why has the whole language reading persisted institutionally despite cognitive science findings (National Reading Panel, Simple View of Reading) documenting superior outcomes for explicit phonics + meaningful engagement combined?',
    'Historical and institutional analysis: examine teacher-training curriculum, professional organization positions, publishing industry incentives, and policy adoption trajectories from 1980-present. Determine whether institutional lock-in, ideological commitment, or genuine disagreement about evidence quality drives persistence.',
    'If institutional lock-in dominates, the constraint''s classification tilts toward snare (persistence depends on excluding contradictory evidence and organizational gatekeeping). If ideological disagreement dominates, it remains tangled_rope (genuine coordination with asymmetric extraction). The mechanism determines whether institutional remedies (retraining, curriculum adoption, evidence integration) would resolve the constraint or whether it requires more forceful de-institutionalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_science_evidence_integration, conceptual, 'The mechanism by which whole language persists despite evidence favoring alternatives.').

omega_variable(
    kernel_reading_coexistence,
    'Can whole language and phonics readings coexist in the same classroom/school system, or do they foreclose each other at the practice level?',
    'Ethnographic study of schools attempting to integrate phonics and whole language; examine whether teachers reconcile the readings (balanced literacy) or whether institutional/ideological pressure forces choice.',
    'If readings coexist, the kernel exhibits institutional plurality and different schools can hold different readings. If they foreclose, one reading''s dominance excludes the other institutionally, and whichever reading dominates (currently whole language in many districts) structures the constraint asymmetrically for non-adopters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether whole language and phonics readings can coexist institutionally or whether they foreclose each other.').

omega_variable(
    suppression_mechanism_institutional_vs_internalized,
    'Is the exclusion of phonics advocates from curriculum policy structural (gatekeeping, professional organization control, publishing leverage) or internalized (teacher belief that phonics is reductive, educational establishment ideological consensus)?',
    'Policy analysis of curriculum adoption processes and teacher surveys on evidence access and professional openness to alternatives. If advocates are excluded by gatekeeping but teachers would adopt phonics if permitted, suppression is structural. If teachers actively reject phonics despite exposure to evidence, suppression is partially internalized.',
    'If suppression is structural, loosening gatekeeping (curriculum choice, evidence integration mandates) may shift institutional practice. If internalized, professional identity and ideological commitment maintain the constraint despite loosened gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_vs_internalized, empirical, 'Whether institutional exclusion of phonics advocates is structural or depends on internalized teacher commitment to whole language ideology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(lite_tr_t35, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(lite_be_t35, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(lite_su_t35, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__whole_language_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel comprises four structurally distinct readings of a single contested commitment: what mechanisms drive reading acquisition and what instructional design follows. Each reading instantiates a different constraint with different beneficiary/victim structures, different ε values, and different classifications. The whole_language_reading privileges meaning and engagement, benefits teachers and print-rich students, and extracts from low-literacy and phonologically vulnerable students. The phonics_reading privileges explicit decoding, extracts from meaning-centered advocates, and benefits students with processing deficits. The balanced_literacy_reading claims integration; the structured_literacy_reading operationalizes systematic phonological instruction. These are NOT alternative observations of the same constraint; they are competing framings with different empirical predictions and institutional consequences. Each story carries its own founding problem (alternative readings of the historical problem of 20th-century reading instruction), its own measurement profile, and its own stakeholder structure. They are linked through the network to indicate institutional competition and empirical entanglement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__whole_language_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
