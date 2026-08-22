% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
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
 *   human_readable: Whole Language Reading Acquisition Doctrine
 *   domain: educational_psychology/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint is the whole-language reading of the contested literacy
 *   acquisition kernel: reading emerges through meaningful engagement with
 *   connected text, decoding develops naturally through exposure and context,
 *   and explicit systematic phonics instruction is treated as unnecessary and
 *   potentially demotivating. The doctrine's coordination function is genuine
 *   — it protects teacher professional judgment and orients instruction
 *   toward authentic literature and comprehension rather than rote drill. Its
 *   extraction falls asymmetrically: students who arrive with strong home
 *   print exposure absorb decoding incidentally and experience the doctrine
 *   as low-cost or beneficial, while students without that background —
 *   including dyslexic learners and English language learners, who
 *   structurally cannot acquire decoding through incidental exposure alone —
 *   bear a compounding, largely invisible cost as reading difficulty is often
 *   reinterpreted as a motivation problem rather than a skill deficit. The
 *   claim is authored as tangled_rope because both a genuine coordination
 *   function (protecting authentic, motivating instruction and professional
 *   autonomy) and asymmetric extraction (deferred decoding failure
 *   concentrated on already-disadvantaged students) are structurally present
 *   and mutually necessary to the arrangement's persistence — this is not a
 *   claim that whole language is illegitimate as a theory, but that its
 *   institutionalized form requires active enforcement (credentialing
 *   pipelines, curriculum adoption, professional identity investment) to
 *   persist against a substantial and growing contrary evidence base.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Acquisition Doctrine").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '84929eb8-a253-408a-9d2d-de50ec850d42').
narrative_ontology:cs_kernel_codification('84929eb8-a253-408a-9d2d-de50ec850d42', distributed).
narrative_ontology:cs_authority_grounding('84929eb8-a253-408a-9d2d-de50ec850d42', practice).
narrative_ontology:cs_interpretation_layer_present('84929eb8-a253-408a-9d2d-de50ec850d42').
narrative_ontology:cs_reading_relation('84929eb8-a253-408a-9d2d-de50ec850d42', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('84929eb8-a253-408a-9d2d-de50ec850d42', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('84929eb8-a253-408a-9d2d-de50ec850d42', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('84929eb8-a253-408a-9d2d-de50ec850d42', foundational, decoding_emerges_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(decoding_emerges_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('84929eb8-a253-408a-9d2d-de50ec850d42', decoding_emerges_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('84929eb8-a253-408a-9d2d-de50ec850d42', secondary, explicit_decoding_drill_undermines_motivation).
narrative_ontology:cs_axiom_status(explicit_decoding_drill_undermines_motivation, holdable).
narrative_ontology:cs_axiom_grounding('84929eb8-a253-408a-9d2d-de50ec850d42', explicit_decoding_drill_undermines_motivation, empirically_contingent).
narrative_ontology:cs_reference_frame('84929eb8-a253-408a-9d2d-de50ec850d42', meaning_centered_emergent_literacy).
narrative_ontology:cs_drift_state('84929eb8-a253-408a-9d2d-de50ec850d42', post_national_reading_panel_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('84929eb8-a253-408a-9d2d-de50ec850d42', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_credentialed_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, schools_of_education_faculty).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, print_rich_home_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, english_language_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in whole-language and balanced-literacy methods through teacher preparation programs; their professional identity, credentials, and classroom autonomy are built around facilitating meaningful text engagement rather than delivering scripted decoding drills. Adopting systematic phonics instruction would require retraining and an implicit admission that prior practice underserved students. They select classroom materials and reading curricula largely by their own professional judgment, which the doctrine explicitly protects.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_credentialed_teachers, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, whole_language_credentialed_teachers, agenda_setter).

% Built entire teacher-preparation curricula, textbooks, and research programs around emergent-literacy and meaning-making theories of reading. Their scholarly output, tenure cases, and institutional prestige are tied to the continued legitimacy of whole-language premises. Reversing course threatens accreditation standing and decades of published theory.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, schools_of_education_faculty, beneficiary,
    institutional, generational, identity_locked, national).

% Arrive at school with extensive at-home reading exposure, vocabulary, and print awareness already in place. For these students, meaning-driven immersion in connected text supplements a decoding foundation they absorb incidentally or receive through supplemental tutoring; the absence of explicit phonics instruction is a minor inconvenience rather than a barrier.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, print_rich_home_students, beneficiary,
    moderate, biographical, mobile, local).

% Depend on school as their only structured source of decoding instruction. When phonics is treated as something that emerges naturally through exposure, these students receive no systematic alternative pathway and fall progressively behind in decoding fluency, which compounds into comprehension deficits by third grade. They and their families have no classroom-level exit — the curriculum is set by the district and teacher, not chosen by the child.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support, payer,
    powerless, biographical, trapped, local).

% Have a specific neurological difficulty mapping print to sound that requires explicit, systematic, cumulative phonics instruction to overcome; incidental exposure to connected text does not build the phonological awareness they need. Under whole-language instruction, their reading difficulty is often misread as a motivation or engagement problem rather than a decoding deficit, delaying identification and intervention for years.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Lack the same first-language oral vocabulary base that whole-language instruction assumes learners bring to text. Meaning-based guessing strategies (context, picture cues) substitute poorly for explicit grapheme-phoneme mapping when the underlying oral language itself is still developing. Families often cannot supplement instruction at home in English.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, english_language_learners, payer,
    powerless, biographical, constrained, local).

% Adopt curricula and enforce them through professional development mandates, textbook purchasing, and administrative oversight of classroom practice. Choices are shaped by state approval lists, teacher union agreements protecting instructional autonomy, and cost of retraining an entrenched workforce; reversing course carries real budget and political costs even where evidence points toward explicit instruction.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, school_district_administrators, agenda_setter,
    institutional, generational, constrained, regional).

% Cognitive scientists and reading researchers whose converging evidence (the National Reading Panel synthesis and subsequent meta-analyses) supports systematic phonics as necessary for most learners are frequently absent from teacher-preparation curricula and district decision-making forums, where schools-of-education faculty and credentialed practitioners retain institutional control over what counts as legitimate pedagogical knowledge.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, reading_researchers_cognitive_science, excluded,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, diffuse).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom practice around a coherent theory of reading as meaning-making, giving teachers professional latitude to select authentic, engaging texts and protecting them from prescriptive scripted curricula; it solves the real problem of reading instruction that is joyless, decontextualized, and disconnected from comprehension and motivation.
% TRANSFER_FUNCTION: Moves instructional time and curricular authority away from systematic decoding instruction and toward text immersion and teacher-selected literature; the deferred cost of unacquired decoding skill is transferred forward onto students who lack outside compensatory literacy support, while professional autonomy and institutional continuity accrue to teachers and teacher-educators.
% ABSENT_VOICES: Parents of struggling readers and dyslexia advocacy organizations are frequently not represented in curriculum-adoption committees dominated by school-of-education-trained staff; cognitive science reading researchers whose decoding-first evidence base is strong are largely outside the teacher-preparation pipeline that trains and licenses classroom teachers.
% DISAPPEARANCE_RATIONALE: If whole-language premises vanished overnight, teacher-preparation curricula, credentialing requirements, textbook markets, and decades of published theory would need wholesale revision; districts would face costly retraining, and a large professional class whose identity and expertise are built on emergent-literacy theory would face devaluation of that expertise — the arrangement is deeply load-bearing for institutional structures, not merely a description of how children read.
% FOUNDING_PROBLEM: Mid-20th-century reading instruction was often reduced to rote, decontextualized drill (isolated letter sounds, basal readers stripped of narrative meaning) that produced technically decodable but disengaged, non-comprehending readers; whole language arose to restore meaning, motivation, and authentic literature to reading instruction.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language advocates and schools-of-education faculty attest the founding problem (joyless, meaning-stripped drill) remains a live risk if phonics returns to prominence. Independent corroboration from outside the beneficiary set — the National Reading Panel (2000), subsequent NICHD-funded meta-analyses, and international dyslexia and cognitive-science research bodies — attests that the founding problem as originally framed was overstated relative to decoding's actual role, and that the whole-language solution itself produces a new, empirically documented harm (delayed decoding acquisition) that the founding narrative does not account for.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction rises across the measured interval (0.32 to 0.58) as the cognitive-science evidence base supporting systematic phonics accumulated (particularly post-2000 National Reading Panel synthesis and subsequent neuroimaging and meta-analytic work), while the institutional apparatus defending whole-language premises persisted and in places hardened — this is the mountain_extraction_accumulation pattern in miniature, though this constraint is not claimed as a mountain. Suppression is comparatively low relative to extraction (0.42 at endpoint) because the doctrine's persistence relies less on coercive exclusion of alternatives and more on credentialing structures, curriculum-adoption inertia, and professional identity investment — soft suppression through institutional channels rather than hard suppression through denial of exit. Theater ratio is moderate and rising (0.12 to 0.31) as 'balanced literacy' framing was increasingly adopted rhetorically by whole-language-trained districts and publishers without correspondingly increasing systematic, explicit phonics instructional time — a Goodhart-style substitution where the label changed faster than the practice.
 *
 * PERSPECTIVAL GAP:
 *   From the teacher and teacher-educator seats, this constraint reads as a rope: professional judgment is preserved, students engage with authentic literature, and the arrangement solves the real coordination problem of joyless, decontextualized drill. From the seat of a dyslexic student or a child without a print-rich home, the same arrangement reads much closer to a snare: the absence of systematic decoding instruction is not a stylistic choice but a structural barrier that, absent parental resources to supplement it, produces persistent illiteracy risk. The engine should compute these as genuinely different per-seat classifications from the same structural data — that divergence is not an error but the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole-language-credentialed teachers and schools-of-education faculty sit near the beneficiary end: the doctrine validates their existing training, protects their classroom autonomy, and requires no costly retraining. Print-rich-home students sit near symmetric-to-beneficiary: they experience negligible cost because their home environment substitutes for the missing systematic instruction. Students without home literacy support, dyslexic students, and English language learners sit near the full-target end: they are structurally trapped (a young child cannot choose their classroom's pedagogy) and their decoding deficit compounds silently for years before diagnosis, if it is diagnosed at all under a framework that treats reading difficulty as an engagement problem rather than a skill gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rote, meaning-stripped, demotivating reading drill — was real when whole language emerged, and the doctrine has never fully lost its coordination function: teacher autonomy and authentic-text engagement remain genuinely valuable and are not obsolete. What has shifted is the evidentiary status of the doctrine's core empirical claim (that phonics develops naturally through exposure), which a substantial, independently corroborated research base now contradicts for most learners and especially for the students least equipped to compensate. Because the coordination function persists alongside now-well-documented asymmetric harm requiring institutional maintenance to sustain, this is not simply an obsolete mandate (which would argue for scaffold or piton) but an active tangled_rope: the doctrine still does real coordination work for the beneficiary seats while extracting, through the same structure, from students who cannot exit the classroom that implements it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_emergence_vs_constructed_pedagogy,
    'Is the claim that phonics develops naturally through exposure and context a genuine empirical finding about typical reading development, or a pedagogical doctrine constructed and sustained by the professional and institutional interests it benefits?',
    'Longitudinal, randomized or quasi-experimental comparison of decoding and comprehension outcomes for matched cohorts taught under whole-language versus systematic-phonics-inclusive curricula, controlling for home literacy environment; convergence or divergence of independent meta-analyses (e.g., National Reading Panel successors, Institute of Education Sciences practice guides) with school-of-education-affiliated research.',
    'If the natural-emergence claim is empirically well-supported for the general population, the extraction on non-print-rich students would need to be explained by implementation failure rather than doctrinal defect, weakening the tangled_rope reading toward scaffold or rope. If the claim is empirically unsupported as a general theory, the tangled_rope reading is strengthened and the doctrine''s persistence is better explained by institutional and professional-identity investment than by pedagogical merit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_emergence_vs_constructed_pedagogy, empirical, 'Whether natural phonics emergence is a genuine developmental finding or an institutionally sustained construct.').

omega_variable(
    reading_kernel_framing_choice,
    'Is the literacy_acquisition_kernel best framed as a single underspecified construct (''how do children learn to read'') with four competing readings, or are the readings better understood as addressing genuinely different sub-populations (e.g., structured_literacy_reading for students with diagnosed dyslexia, whole_language_reading for typically-developing readers with rich home literacy support) such that the ''contest'' partly dissolves once population is specified?',
    'Population-stratified outcome data comparing whole-language and structured-literacy approaches separately for typically-developing print-rich-home students versus students with diagnosed reading disabilities or under-resourced home literacy environments.',
    'If the readings are genuinely population-specific rather than universally competing, this reading''s high extraction on non-print-rich and dyslexic students may be better modeled as a scope-mismatch problem (applying a population-appropriate method universally) than as extraction inherent to the doctrine itself — this would not change ε for THIS reading as authored (which describes the doctrine''s actual universal application in most U.S. elementary classrooms) but would inform how the four sibling stories are related in commentary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_framing_choice, conceptual, 'Whether the kernel contest is a genuine universal disagreement or a population-scope mismatch across readings.').

omega_variable(
    teacher_professional_identity_lock_mechanism,
    'Is teacher and teacher-educator resistance to phonics-based reform driven primarily by genuine pedagogical conviction, by professional identity investment (retraining costs, implicit admission of past harm), or by structural incentives within schools of education (tenure, publication records, program accreditation)?',
    'Survey and interview research distinguishing teachers'' stated pedagogical reasoning from institutional incentive structures; tracking whether districts that mandate retraining see resistance decline once retraining costs are institutionally absorbed (suggesting incentive-driven resistance) versus persist (suggesting genuine conviction-driven resistance).',
    'If resistance is primarily incentive-structural, the identity_locked exit_options classification for teachers and school-of-education faculty is well-supported and the constraint''s persistence is substantially inertial/institutional. If resistance is primarily genuine pedagogical conviction independent of institutional stake, the beneficiary classification should be read as good-faith rather than self-interested, which does not change ε but changes how the mandatrophy_analysis should characterize the doctrine''s persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_professional_identity_lock_mechanism, conceptual, 'Distinguishing genuine conviction from institutional identity-lock in teacher resistance to reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(lite_tr_t32, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(lite_be_t32, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(lite_su_t32, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__whole_language_reading, 0.1).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of literacy_acquisition_kernel, decomposed per the ε-invariance principle because 'how children learn to read' conflates structurally distinct empirical and normative claims with different beneficiary/victim structures. whole_language_reading (this file) authors low extraction on teacher autonomy and high extraction on non-print-rich and dyslexic students, claimed as tangled_rope. phonics_reading and structured_literacy_reading are expected to author lower extraction on struggling-reader populations but higher friction/extraction on teacher autonomy (mandated scripted curricula). balanced_literacy_reading sits structurally between, claiming to resolve the contest but empirically often defaulting toward whole-language-dominant classroom practice despite phonics-inclusive framing — its own story should document that gap rather than this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
