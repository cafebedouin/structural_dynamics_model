% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Doctrine
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint models the whole-language reading acquisition doctrine as
 *   an institutionalized instructional arrangement within American (and
 *   internationally adopted) teacher preparation and curriculum systems: the
 *   claim that decoding ability emerges implicitly from meaningful engagement
 *   with authentic texts, without need for a systematic, sequenced program of
 *   explicit phonics instruction. This is one reading of a contested kernel
 *   about how reading acquisition works; the sibling readings (phonics-first,
 *   balanced literacy) are separate constraint stories with their own ε and
 *   stakeholder structures, not alternate observables of this one. The
 *   extractiveness authored here (0.62) reflects the arrangement's own
 *   operation as this reading's proponents and critics jointly observe it —
 *   rising over time as cognitive-science evidence accumulated and
 *   remediation costs for struggling readers became visible, not the ε that a
 *   phonics-based alternative would carry.
 *
 * KEY AGENTS:
 *   - credentialed_literacy_faculty: Primary agenda-setter (institutional/arbitrage) — trains teachers, sets accreditation norms, insulated from outcome accountability
 *   - whole_language_curriculum_publishers: Beneficiary (organized/mobile) — sells authentic-text and leveled-reader programs
 *   - classroom_teachers_valuing_autonomy: Mixed beneficiary/payer (moderate/constrained) — gains professional discretion, bears blame for poor outcomes
 *   - struggling_decoders, dyslexic_students, low_income_students_without_home_literacy_support: Primary targets (powerless/trapped) — bear cumulative reading-skill deficits
 *   - reading_researchers_cognitive_science: Excluded analytical voice — evidence base largely external to the credentialing pipeline
 *   - state_education_departments: Institutional observer with authority to intervene
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.62).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.55).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Doctrine").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, 'aff8aa64-2449-4964-9c4f-82b5a40b6075').
narrative_ontology:cs_kernel_codification('aff8aa64-2449-4964-9c4f-82b5a40b6075', distributed).
narrative_ontology:cs_authority_grounding('aff8aa64-2449-4964-9c4f-82b5a40b6075', expertise).
narrative_ontology:cs_interpretation_layer_present('aff8aa64-2449-4964-9c4f-82b5a40b6075').
narrative_ontology:cs_reading_relation('aff8aa64-2449-4964-9c4f-82b5a40b6075', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('aff8aa64-2449-4964-9c4f-82b5a40b6075', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('aff8aa64-2449-4964-9c4f-82b5a40b6075', foundational, decoding_emerges_implicitly_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('aff8aa64-2449-4964-9c4f-82b5a40b6075', decoding_emerges_implicitly_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('aff8aa64-2449-4964-9c4f-82b5a40b6075', secondary, authentic_text_engagement_is_necessary_and_sufficient_for_literacy_motivation).
narrative_ontology:cs_axiom_status(authentic_text_engagement_is_necessary_and_sufficient_for_literacy_motivation, holdable).
narrative_ontology:cs_axiom_grounding('aff8aa64-2449-4964-9c4f-82b5a40b6075', authentic_text_engagement_is_necessary_and_sufficient_for_literacy_motivation, instrumental).
narrative_ontology:cs_reference_frame('aff8aa64-2449-4964-9c4f-82b5a40b6075', meaning_centered_literacy_movement).
narrative_ontology:cs_drift_state('aff8aa64-2449-4964-9c4f-82b5a40b6075', post_national_reading_panel_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aff8aa64-2449-4964-9c4f-82b5a40b6075', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, credentialed_literacy_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_valuing_autonomy).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_decoders).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, low_income_students_without_home_literacy_support).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_valuing_autonomy).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, constructivist_learning_theory).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, teacher_professional_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains new teachers in schools of education, writes methods textbooks, and sets accreditation expectations around meaning-centered reading instruction. Careers, publications, and program identities are built on the whole-language framework; their professional standing does not depend on classroom-level reading outcomes being tracked against it.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, credentialed_literacy_faculty, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell leveled-reader libraries, guided-reading kits, and authentic-text programs adopted district-wide under whole-language mandates. Revenue depends on continued adoption; they can pivot product lines if the instructional consensus shifts, unlike the students inside classrooms using the materials.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Trained under whole-language philosophy, they experience it as professionally respectful — no scripted phonics sequence to follow, discretion to select texts and respond to individual children. When students fail to decode, they often lack a systematic alternative to fall back on, and are then blamed for implementation rather than the framework being questioned.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_valuing_autonomy, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_valuing_autonomy, payer).

% Children who do not spontaneously infer grapheme-phoneme correspondence from exposure to authentic texts. Without explicit instruction they plateau, are often mislabeled as having attention or motivation problems, and accumulate reading deficits that compound across grades. They have no say in curriculum and cannot exit the classroom.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_decoders, payer,
    powerless, biographical, trapped, local).

% Students whose neurocognitive profile makes implicit phonological inference from context especially unreliable; the approach that assumes decoding emerges from meaning-exposure is close to the opposite of what remediation research (e.g., structured literacy) recommends for this population. They bear the most severe and hardest-to-reverse costs.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Students whose home environments do not supply the incidental print exposure and vocabulary scaffolding the approach implicitly assumes. Peers with print-rich homes partially compensate for the missing explicit instruction; these students do not, widening achievement gaps that track family resources rather than aptitude.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, low_income_students_without_home_literacy_support, payer,
    powerless, biographical, trapped, local).

% Produce converging experimental and neuroimaging evidence (National Reading Panel, meta-analyses) that decoding is not typically acquired implicitly for most learners and requires explicit, systematic phonics instruction. Their findings circulate in journals but historically had limited uptake in schools-of-education curricula, which operate on a separate institutional track.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, reading_researchers_cognitive_science, excluded,
    institutional, generational, analytical, national).

% Set literacy standards and approve curricula; increasingly commission audits comparing whole-language-trained cohorts against phonics-based cohorts on reading assessments, and can mandate structured literacy requirements that override local instructional autonomy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, state_education_departments, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__whole_language_reading, credentialed_literacy_faculty).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom practice around the idea that reading is best taught through rich, motivating, authentic literature rather than isolated skill drills — solving a real problem of disengagement and rote, meaning-stripped instruction that earlier basal-reader phonics programs sometimes produced.
% TRANSFER_FUNCTION: Moves instructional time, curriculum-adoption dollars, and professional legitimacy toward literacy faculty and publishers invested in meaning-centered pedagogy, while moving the cost of undiagnosed decoding failure onto individual struggling readers, their families, and downstream remediation systems (special education referrals, tutoring markets, grade retention).
% ABSENT_VOICES: Reading science researchers whose experimental and neuroimaging findings on decoding acquisition are not systematically incorporated into teacher preparation programs; parents of struggling readers who lack the specialized vocabulary to challenge a framework endorsed by credentialed experts; the students themselves, who have no voice in curriculum adoption and often cannot self-diagnose why they are falling behind.
% DISAPPEARANCE_RATIONALE: If the framework disappeared overnight, teacher preparation programs would need to restructure literacy coursework, publishers would need to retool curriculum lines toward structured/systematic phonics sequences, and a large population of teachers currently credentialed under this philosophy would need retraining — an actual institutional and market reorganization, not a null event.
% FOUNDING_PROBLEM: Mid-20th-century basal reading programs were often criticized as tedious, decontextualized, and disconnected from genuine comprehension and love of reading; whole language arose to solve disengagement and to center meaning-making over rote phonics drills.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science and reading-research bodies (e.g., National Reading Panel synthesis, subsequent meta-analytic literacy research) attest from outside the schools-of-education establishment that the founding problem of engagement was real but that the proposed mechanism (implicit decoding acquisition) is not empirically supported for most learners; several state education departments corroborate this externally through post-hoc assessment audits showing decoding-skill deficits correlated with whole-language-dominant instruction. No corroboration for the mechanism's validity has emerged from outside the credentialed literacy-faculty community itself.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects a structural transfer: instructional time and system-level legitimacy accrue to literacy faculty and publishers whose professional and commercial standing depend on the framework's continued adoption, while the cost of undiagnosed decoding failure lands on students least able to compensate (dyslexic students, students without literacy-rich homes). Suppression (0.55) is moderate — no legal mandate forces the approach, but accreditation gatekeeping, textbook market consolidation, and professional-identity effects make alternative instructional models hard for individual teachers to adopt even when they suspect a problem. Theater ratio (0.48) is elevated because much continued advocacy for meaning-centered, authentic-text-only instruction persists as professional-identity performance (rejecting anything resembling 'drill and kill' phonics) even as empirical support for the implicit-acquisition mechanism has weakened — a genuine Goodhart-style substitution of ideological commitment for demonstrated decoding outcomes. Accessibility collapse (0.40) is moderate: unlike a mountain, alternative instructional models (systematic phonics, structured literacy) remain visible and increasingly mandated by state policy, so alternatives have not fully collapsed. Resistance (0.60) is substantial and rising — reading-science advocacy movements, dyslexia parent organizations, and state legislative reform efforts actively contest the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (literacy faculty), the arrangement is a defensible pedagogical philosophy under continued professional refinement. From the payer seat (struggling decoders, dyslexic students), the same arrangement computes as an extractive structure that denies them evidence-based instruction while treating their failure to learn as a personal or developmental deficiency rather than an instructional one. The engine should register significant seat divergence here — this is exactly the case the classification framework is designed to surface, not one to explain away.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed literacy faculty and curriculum publishers sit near the full-beneficiary end: they collect professional and commercial value from the framework's persistence and hold arbitrage/mobile exit options allowing them to reposition if the framework loses ground. Classroom teachers are dual-positioned — beneficiaries of professional autonomy but partial payers when blamed for outcomes they were not equipped to prevent; their exit is constrained by sunk training investment. Struggling decoders, dyslexic students, and low-income students without home literacy support are full targets: they are trapped inside the arrangement (they do not choose their instructional method), bear compounding costs across school years, and have no institutional voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — disengaging, rote basal-reader instruction — was real and the meaning-centered response solved a genuine motivational and engagement deficit. But the specific mechanism claim (decoding emerges implicitly from exposure) has substantial contrary evidence from reading science research accumulated since the 1990s-2000s, while the institutional infrastructure built around the mechanism (teacher training pipelines, publisher contracts, professional identity) persists independent of whether the mechanism claim holds. This is the classic mandatrophy signature: a coordination function (engaging literacy instruction) has outlived its founding empirical justification (implicit decoding acquisition) but continues under institutional inertia and identity investment. Classifying this as tangled_rope rather than snare preserves the genuine coordination value (many meaning-centered classroom practices remain valuable independent of the decoding-mechanism claim) while still registering the asymmetric extraction imposed on struggling readers — a pure snare classification would erase the real coordination history; a rope classification would erase the documented victim population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_acquisition_empirical_status,
    'Does decoding ability genuinely emerge implicitly from exposure to authentic texts for a meaningful subpopulation of learners, or is the near-universal need for explicit phonics instruction the accurate empirical picture?',
    'Large-scale longitudinal comparison of reading outcomes between whole-language-instructed and structured-literacy-instructed cohorts, controlling for socioeconomic background and disaggregated by phonological processing profile; converging neuroimaging and cognitive science evidence on typical decoding acquisition pathways.',
    'If implicit acquisition holds for a substantial subpopulation, the framework''s coordination function (protecting engagement and authentic literacy motivation) is better justified and the extraction/victim framing should be narrowed to the subpopulation for whom it demonstrably fails. If it does not hold broadly, the tangled_rope classification understates the case for reclassification toward snare for the general population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_acquisition_empirical_status, empirical, 'Whether the implicit-decoding-acquisition mechanism is empirically supported for most or only some learners.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the whole_language, phonics, and balanced_literacy readings of the reading-acquisition kernel diverge, and is the disagreement about the acquisition MECHANISM or about optimal INSTRUCTIONAL SEQUENCING given an agreed mechanism?',
    'Structural comparison of each reading''s founding_problem, beneficiary/victim declarations, and axioms — documented across the three linked constraint stories in this family via network.affects_constraints.',
    'If the disagreement is purely about sequencing (all three readings agree decoding requires some systematic input, differing only on how integrated with authentic-text exposure it should be), the readings would coexist rather than foreclose one another. If the disagreement is about the underlying acquisition mechanism itself (implicit vs. explicit), the whole_language and phonics readings may be closer to a foreclosing relationship, since the core premise of one denies the necessity claim of the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural point of disagreement among the three kernel readings.').

omega_variable(
    teacher_professional_identity_lock,
    'To what extent is continued adoption of whole-language methods among individual classroom teachers driven by identity-lock (professional training and self-concept invested in the meaning-centered philosophy) versus genuine ongoing belief in its empirical validity?',
    'Survey and interview research tracking teacher attitude change following exposure to structured-literacy retraining programs and reading-science evidence; measure adoption rates and resistance patterns.',
    'If identity-lock dominates, the classroom-teacher stakeholder seat''s directionality and exit_options should be understood as more constrained than a simple belief-revision model would suggest, and mandated retraining interventions would face higher resistance independent of evidence quality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_professional_identity_lock, empirical, 'Whether teacher persistence with the framework reflects identity investment or genuine evidentiary conviction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 32, 0.45).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(read_be_t32, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(read_su_t32, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__whole_language_reading, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the reading_acquisition_mechanism kernel. whole_language_reading (this story) authors high and rising extractiveness with an identifiable powerless victim population (struggling decoders, dyslexic students, under-resourced students). phonics_reading is expected to author near-mountain metrics given convergent cognitive-science support for explicit code instruction. balanced_literacy_reading is expected to author a lower-extraction, more genuinely coordination-heavy hybrid profile. Each story's ε is authored independently per its own reading's structural claim about how reading acquisition works; they are not measurements of the same constraint from different angles, but three structurally distinct arrangements sharing a genealogical origin in a real 20th-century pedagogical dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
