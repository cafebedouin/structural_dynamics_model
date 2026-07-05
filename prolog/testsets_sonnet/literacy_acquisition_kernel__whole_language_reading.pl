% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Whole Language Reading Acquisition Doctrine
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story models the whole-language reading of the literacy acquisition
 *   kernel: reading as a natural, meaning-driven process analogous to oral
 *   language, where explicit phonics instruction is unnecessary and
 *   potentially counterproductive to motivation. The claim of a coordination
 *   function (restoring authentic, motivating literacy instruction and
 *   preserving teacher professional judgment) is real and historically
 *   grounded. But the same structure that protects teacher autonomy and
 *   rewards students with strong home literacy backgrounds systematically
 *   fails students who depend on the classroom for explicit decoding
 *   instruction — dyslexic students, English language learners, and children
 *   without print-rich homes. As the approach became institutionally
 *   entrenched in teacher preparation programs and leveled-reader publishing
 *   over decades, the extraction from underserved student populations
 *   accumulated alongside growing theater in the form of assessment
 *   workarounds (running records, cueing-strategy checklists) that
 *   increasingly substitute for direct measurement of decoding skill.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Acquisition Doctrine").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, 'c3bd41b9-71dc-492e-8a5b-a747e36cbbdd').
narrative_ontology:cs_kernel_codification('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', distributed).
narrative_ontology:cs_authority_grounding('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', practice).
narrative_ontology:cs_interpretation_layer_present('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd').
narrative_ontology:cs_reading_relation('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', foundational, decoding_emerges_naturally_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(decoding_emerges_naturally_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', decoding_emerges_naturally_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', secondary, explicit_decoding_instruction_harms_motivation).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_harms_motivation, holdable).
narrative_ontology:cs_axiom_grounding('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', explicit_decoding_instruction_harms_motivation, empirically_contingent).
narrative_ontology:cs_reference_frame('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', emergent_meaning_centered_literacy).
narrative_ontology:cs_drift_state('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', post_science_of_reading_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c3bd41b9-71dc-492e-8a5b-a747e36cbbdd', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, print_rich_home_students).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_teacher_educators).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, classroom_teachers_professional_autonomy).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, english_language_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Selects leveled readers, running records, and meaning-cueing strategies rather than scripted phonics programs, preserving professional judgment about how each child is taught. Trained in whole-language teacher preparation programs and evaluated partly on adherence to this philosophy; switching to systematic phonics instruction would require retraining and would implicitly indict prior practice.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, classroom_teachers_professional_autonomy, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, classroom_teachers_professional_autonomy, agenda_setter).

% Schools of education, textbook publishers of leveled-reader systems, and literacy consultants whose curricula, certifications, and publishing lines are built on emergent-literacy theory. Their professional and financial standing is bound to the continued acceptance of this reading; reversing course threatens accreditation pipelines and consulting revenue built over decades.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_teacher_educators, beneficiary,
    institutional, generational, identity_locked, national).

% Children who arrive at school with extensive prior exposure to books, print conventions, and vocabulary from home. For these children, immersion in connected text and meaning-cueing largely works because the phonics gap is filled invisibly by background knowledge and parental scaffolding; they experience the approach as effective and motivating.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, print_rich_home_students, beneficiary,
    powerless, biographical, mobile, local).

% Children without extensive home print exposure depend on the classroom to supply explicit decoding instruction, which this approach withholds or minimizes on principle. They fall behind in word recognition, are often reclassified as unmotivated or low-ability rather than under-instructed, and cannot exit the assigned classroom or curriculum.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support, payer,
    powerless, biographical, trapped, local).

% Children with phonological processing difficulties require explicit, systematic, cumulative phonics instruction to compensate; guessing-from-context and meaning-cueing strategies actively obscure their decoding deficits, delaying identification and appropriate intervention for years. They cannot select their instructional method and often are diagnosed only after prolonged failure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia, payer,
    powerless, biographical, trapped, local).

% Students acquiring English alongside literacy lack the deep vocabulary and syntactic intuitions that meaning-cueing strategies assume; without explicit grapheme-phoneme instruction they cannot bootstrap decoding from context they do not yet possess linguistically.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, english_language_learners, payer,
    powerless, biographical, trapped, local).

% Adopt district-wide curricula and reading series, often on the recommendation of literacy consultants trained in this tradition. Bear reputational and legal exposure when reading scores lag but face resistance from entrenched teacher-training pipelines when proposing systematic-phonics adoption.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, school_district_administrators, agenda_setter,
    institutional, biographical, constrained, regional).

% Cognitive scientists and reading researchers whose converging evidence (the 'science of reading') on phonological processing and decoding has been documented since the 1990s but was for decades excluded from teacher preparation curricula and district policy conversations dominated by whole-language-trained literacy faculty.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, reading_science_researchers, excluded,
    organized, generational, analytical, national).

% Discover their children cannot decode unfamiliar words despite years of schooling, often only after seeking private tutoring or evaluation. Rarely consulted on instructional method selection and often told their child's difficulty is developmental or motivational rather than a product of missing explicit instruction.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, excluded,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom practice around a coherent, teachable philosophy of literacy development that treats reading as analogous to oral language acquisition, allowing teachers to organize instruction around authentic texts and child-centered engagement rather than isolated skill drills.
% TRANSFER_FUNCTION: Moves instructional time and curricular resources away from explicit, systematic phonics instruction and toward literature immersion, leveled readers, and meaning-cueing strategy training; the cost of the missing explicit instruction is transferred to students who lack the home literacy background needed to compensate, and downstream to remediation systems and families who fund private tutoring.
% ABSENT_VOICES: Reading science researchers documenting phonological-processing evidence were largely excluded from teacher-preparation curricula and district decision-making for decades; parents of struggling readers were rarely consulted and often had their concerns reframed as developmental variation rather than instructional gaps.
% DISAPPEARANCE_RATIONALE: Teacher educators, credentialing pipelines, and leveled-reader publishers would need to substantially restructure if this reading vanished — a real institutional rearrangement. But many practicing teachers already blend cueing strategies with informal phonics, so from the classroom-practice level some argue the world would barely change; the dispute over which counterfactual is real is itself part of the ongoing reading wars.
% FOUNDING_PROBLEM: Mid-20th-century reading instruction relied heavily on rote, decontextualized phonics drills (e.g., 'Dick and Jane' primers) that produced technically decodable but disengaged readers; the whole-language movement was built to restore meaning, motivation, and authentic literature to reading instruction.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language teacher educators and practicing teachers attest the motivation problem remains live and that skill-and-drill phonics still risks disengagement. Independent reading-science researchers and national reading panels (outside the whole-language teacher-training establishment) attest that the founding problem was real but the proposed solution overcorrected, producing a documented decoding-skills gap corroborated by decades of standardized assessment data and cognitive science research not funded or produced by whole-language advocacy institutions.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, contested).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects substantial but not total appropriation: the approach genuinely serves print-rich-home students well while imposing real, documented costs on students lacking that background. Suppression (0.42) is moderate — the mechanism operates less through coercive enforcement of a single family and more through institutional gatekeeping (teacher-certification requirements, curriculum adoption cycles, credentialing pipelines) that makes alternative approaches hard to access even for willing teachers. Theater ratio (0.40) captures the accumulation of cueing-strategy assessment rituals (running records, three-cueing checklists) that increasingly stand in for direct measurement of phonemic decoding ability as the evidence against the approach mounted. Accessibility collapse (0.35) is moderate-low because alternative approaches (structured literacy, phonics-first) remain available and increasingly adopted, unlike a true mountain. Resistance (0.55) is substantial and rising, driven by the science-of-reading movement and parent advocacy.
 *
 * PERSPECTIVAL GAP:
 *   From the classroom-teacher seat, this reads as coordination: a coherent philosophy that respects professional judgment and produces engaged, motivated readers among most students. From the seat of a student without home literacy support or with dyslexia, the same structure operates as extraction — years of instructional time in which explicit decoding is never taught, cascading into remediation, diagnosis delay, and self-concept as a 'poor reader' rather than an under-instructed one. This divergence is the tangled-rope signature: genuine coordination function for some, real extraction for others, held together by institutional enforcement (certification requirements, curriculum adoption) rather than universal benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Teachers and teacher-educator institutions sit near the beneficiary end: they retain professional autonomy, curricular authority, and — for teacher-training institutions — accreditation and publishing revenue streams built on this theory. Print-rich-home students are incidental beneficiaries: the approach works for them because their environment compensates for what the method withholds. Students without home literacy support, dyslexic students, and English language learners sit at the target end: trapped in assigned classrooms, unable to select their instructional method, and bearing the compounding cost of a skill gap that widens each year it goes uncorrected.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rote, meaning-stripped phonics drilling that alienated readers — was genuinely live in the mid-20th century. Whether it remains live today, or whether the corrective overcorrected into a different but equally serious harm (a documented decoding-skills gap for underserved populations), is exactly the contested question the six-questions genealogy surfaces. Corroboration split cleanly along institutional lines: whole-language educators (benefiting parties) attest continuity of the founding problem, while independent reading-science researchers and national assessment data (outside the benefiting institutions) attest the corrective has itself become extractive for a a specific victim population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergent_literacy_naturalness_ambiguity,
    'Is reading acquisition genuinely analogous to oral language acquisition (an emergent, naturally-developing skill that explicit instruction can distort), or is decoding a culturally invented skill that requires explicit instruction regardless of a child''s engagement with meaningful text?',
    'Convergent evidence from cognitive neuroscience on reading circuitry (the visual word form area is not evolutionarily dedicated to reading, unlike oral language regions), and randomized controlled trials comparing systematic phonics instruction against pure immersion approaches across varied home-literacy-background populations.',
    'If decoding is not naturally emergent the way oral language is, the entire whole-language coordination claim (that phonics develops on its own through exposure) is empirically false rather than merely contested, and the extraction from students without background compensation is not incidental but structurally guaranteed by the approach''s design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergent_literacy_naturalness_ambiguity, empirical, 'Whether reading is naturally emergent like oral language or a taught skill requiring explicit instruction.').

omega_variable(
    kernel_reading_selection_rationale,
    'Given that the literacy_acquisition_kernel supports at least four structurally distinct readings (whole_language, phonics, balanced_literacy, structured_literacy), what determines which reading a given school district, teacher-preparation program, or country adopts, and is that selection process itself evidence-responsive or path-dependent on institutional inertia?',
    'Comparative policy analysis of jurisdictions that have formally shifted reading (e.g., mandated ''science of reading'' legislation) versus those that have not, tracking whether the shift correlates with new evidence or with turnover in teacher-education leadership and textbook adoption cycles.',
    'If reading selection is primarily path-dependent on institutional inertia rather than evidence-responsive, the whole-language reading''s persistence in many teacher-preparation programs is better explained as institutional lock-in than as an ongoing rational assessment of the underlying cognitive science — strengthening the case that teacher_educators'' beneficiary status is protecting sunk institutional investment rather than genuine pedagogical coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_rationale, conceptual, 'Whether reading-kernel selection across institutions tracks evidence or institutional path-dependency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(lite_tr_t32, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(lite_be_t32, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(lite_su_t32, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__whole_language_reading, 0.08).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'how children learn to read' per the ε-invariance principle: whole_language_reading (this file), phonics_reading, balanced_literacy_reading, and structured_literacy_reading each claim a structurally distinct account of literacy acquisition with different ε, different beneficiary/victim structure, and different classification. They are linked as a kernel family via cs_structure.reading_relations and network.affects_constraints rather than merged into one constraint with an observable-dependent ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
