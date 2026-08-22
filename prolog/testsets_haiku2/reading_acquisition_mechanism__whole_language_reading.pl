% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Whole Language Reading Pedagogy — Implicit Decoding Acquisition
 *   domain: educational/pedagogical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel about how
 *   reading is acquired. The whole-language reading claims that meaningful
 *   engagement with authentic texts produces implicit decoding skill without
 *   need for explicit phonics instruction. This reading grounds its
 *   legitimacy in language acquisition theory (children acquire grammar
 *   implicitly through exposure) and in pedagogical equity (authentic
 *   literature centers student voice and interest). The competing readings —
 *   phonics-first and balanced-literacy — disagree on the necessity of
 *   explicit decoding instruction and on who benefits from each approach.
 *   This story describes the whole-language reading as a constraint on
 *   student and teacher behavior: it requires teachers to organize around
 *   literature engagement rather than phonics sequence, and it produces
 *   disproportionate harm to struggling readers when implicit exposure fails.
 *   The constraint's persistence depends on institutional authority (reading
 *   specialists, teacher training programs, textbook publishers) actively
 *   suppressing the phonics school and maintaining the whole-language
 *   framework despite accumulating evidence of its differential harm.
 *
 * KEY AGENTS:
 *   - teacher_practitioners: moderate power, identity-locked agenda-setters; benefit from autonomy; depend on the reading's institutional authority for legitimacy
 *   - struggling_readers: powerless, trapped; bear the cost of implicit-exposure failure; excluded from the decision-making that shapes their instruction
 *   - dyslexic_students: powerless, trapped; neurocognitive characteristics make implicit exposure systematically insufficient; receive late remediation after years of failure
 *   - lower_socioeconomic_background_students: powerless, trapped; lack home literacy foundation that the reading model implicitly presumes
 *   - reading_specialists_ideological_school: institutional power; agenda-setters; benefit from authority and career advancement within the paradigm; have professional identity investment in the reading
 *   - phonics_research_school: institutional power, excluded; produce empirical evidence for alternative approaches; structurally locked out of curriculum authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.68).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.72).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Pedagogy — Implicit Decoding Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational/pedagogical").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c').
narrative_ontology:cs_kernel_codification('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', distributed).
narrative_ontology:cs_authority_grounding('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', extraction).
narrative_ontology:cs_interpretation_layer_present('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c').
narrative_ontology:cs_reading_relation('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', foundational, implicit_exposure_sufficient).
narrative_ontology:cs_axiom_status(implicit_exposure_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', implicit_exposure_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', foundational, authentic_text_primacy).
narrative_ontology:cs_axiom_status(authentic_text_primacy, holdable).
narrative_ontology:cs_axiom_grounding('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', authentic_text_primacy, deontological).
narrative_ontology:cs_reference_frame('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', language_acquisition_implicit_framework).
narrative_ontology:cs_drift_state('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', contemporary_phonics_research_accumulation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3260dcf8-b5d9-4b5e-91c2-0cdf32482c9c', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teacher_practitioners).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, literature_textbook_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, reading_specialists_ideological_school).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, lower_socioeconomic_background_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control classroom reading instruction and text selection. Benefit from low planning overhead (no sequenced curriculum to implement) and professional autonomy in choosing authentic literature. Their professional identity is invested in the whole-language framework after years of training; departing would require identity reconstruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, teacher_practitioners, agenda_setter,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, teacher_practitioners, beneficiary).

% Cannot exit compulsory schooling. Experience less explicit grapheme-phoneme instruction than phonics approaches provide. When implicit exposure fails, they accumulate reading difficulty, delayed remediation, and shame. Entirely dependent on teacher's instructional method.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, national).

% Have neurocognitive characteristics (phonological processing deficits, rapid naming weaknesses) that make implicit exposure systematically insufficient. Require explicit, structured phonics but receive exposure-based instruction. Accumulate years of reading failure before special education identification; develop secondary emotional and behavioral effects.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, dyslexic_students, payer,
    powerless, biographical, trapped, national).

% Often lack home literacy exposure (fewer books, less shared reading) that implicit models assume. Rely entirely on classroom instruction for reading exposure. Whole-language pedagogy is least equipped to substitute for missing home literacy foundation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, lower_socioeconomic_background_students, payer,
    powerless, biographical, trapped, national).

% Benefit from high adoption of literature anthologies and chapter books (authentic texts the reading requires). Market share depends on teacher adoption of the whole-language paradigm. Contribute research funding to sympathetic scholars.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, literature_textbook_publishers, beneficiary,
    institutional, generational, mobile, national).

% Establish research programs, train teachers, publish in prestigious journals, set curriculum standards. Benefit from institutional authority, citation, career advancement. Professional identity bound to the paradigm; shifting would require acknowledging error and ceding authority.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, reading_specialists_ideological_school, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, reading_specialists_ideological_school, beneficiary).

% Competing school of reading research emphasizing explicit phonics produces empirical evidence for phonics efficacy but is largely excluded from curriculum-setting authority. Their alternative framework is not admitted to the authoritative reading; structurally locked out of institutions that set pedagogy standards.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, phonics_research_school, excluded,
    institutional, generational, constrained, national).

% Observe children's reading trajectories; frustrated when implicit exposure fails to produce decoding skills. Can exit through private school or tutoring but most constrained by cost and geography. Voice often dismissed as lacking pedagogical expertise.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, observer,
    moderate, biographical, mobile, national).

% Manage downstream cost of reading failure: identifying, assessing, remediating students who did not acquire reading skills. Operate after constraint has already applied for 2–3 years, making intervention more expensive and less effective.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, special_education_administrators, observer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__whole_language_reading, reading_specialists_ideological_school).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes classroom reading instruction around authentic literature and student-centered meaning-making, solving for teacher autonomy in text selection and avoiding the administrative overhead of sequenced phonics curricula. Creates a reading identity centered on engagement with 'real books' rather than decodable primers.
% TRANSFER_FUNCTION: Transfers the cost of reading failure (remediation, special education referral, time lost to non-fluency) from the general student population to struggling readers, dyslexic readers, and low-literacy-background readers. Transfers the benefit of professional autonomy and identity investment from students to teachers and from struggling readers to those who learn under exposure-based models.
% ABSENT_VOICES: Struggling readers themselves cannot articulate their needs within the pedagogical debate (they are children in classrooms, not participants in curriculum decisions). Dyslexic students are structurally absent from the decision-making that shapes their instruction. Parents who would advocate for alternative instruction are not present at curriculum-committee tables. The phonics research community is largely excluded from the authoritative conversation.
% DISAPPEARANCE_RATIONALE: If the whole-language reading and its enforcement machinery disappeared, classroom reading instruction would reorganize around explicit phonics sequence, reading recovery programs would shift earlier and prevent rather than remediate failure, teacher training would require systematic phonics pedagogy, and textbook adoption would emphasize decodable materials. The distribution of reading success would shift as the cost of implicit-exposure failure transferred back to the general population rather than concentrating on struggling readers.
% FOUNDING_PROBLEM: Early conventional reading instruction was rigid, decontextualized, often divorced from literature and student interest. Phonics drill felt meaningless to learners and relied on artificial decodables rather than authentic, engaging texts. The constraint was established to solve for student engagement, meaning-making, and literate identity development — to make reading feel purposeful rather than mechanical.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language advocates attest the founding problem remains live: conventional phonics instruction can be joyless and disconnected from meaning. Phonics researchers and special educators attest the problem has been solved in balanced-literacy and modern synthetic-phonics approaches, which integrate explicit decoding with authentic literature; they argue the constraint persists as professional ideology rather than necessity. Reading specialists in special education (outside the whole-language research school) report that struggling readers need explicit phonics; this testimony comes from practitioners who work with failure cases.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.48 to 0.68 over the interval as the reading's application expands and the cost of reading failure accumulates. Early in the interval (0–5), extractiveness is lower because the constraint operates in classrooms where students have diverse reading profiles; many learn to read under any reasonable approach. As time accumulates (10–40), extractiveness rises because struggling readers experience cumulative failure, special education costs mount, and the reading's persistent application despite contrary evidence becomes clear. Theater ratio climbs from 0.28 to 0.41 because the justification for implicit exposure shifts from genuine pedagogical philosophy to institutional inertia: as research accumulates showing explicit phonics benefits for struggling readers, the whole-language defense increasingly takes theatrical form (claims about joy and engagement, resistance to research evidence). Suppression rises from 0.58 to 0.72 because the reading's persistence requires active exclusion of phonics school research, resistance to curriculum reform, and counter-evidence suppression at the institutional level. The claim is tangled_rope because the reading genuinely coordinates student engagement with literature (coordination function, real benefit to many students) AND extractively transfers reading failure costs to struggling readers (asymmetric extraction, active enforcement required to prevent alternative instruction). The constraint is NOT a snare because the coordination function is real — the disagreement is not about whether the function exists, but about whether that function justifies the cost borne by struggling readers.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (teacher/specialist) seat and the struggling-reader seat should compute different types from identical structural data. The teacher sees coordination (authentic literature, student engagement); the struggling reader sees extraction (failed exposure, remediation, shame). The institutional-beneficiary seat sees the constraint as rope or even mountain (natural pedagogy); the excluded-research seat sees it as snare (institutional gatekeeping). This divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: teacher_practitioners and reading_specialists_ideological_school sit near d=0.15–0.25 (beneficiaries with institutional power and exit options; the constraint subsidizes their autonomy and authority). Literature_textbook_publishers sit near d=0.20 (beneficiary with institutional power but mobile exit options). Victim directionality: struggling_readers, dyslexic_students, and lower_socioeconomic_background_students sit near d=0.85–0.95 (powerless, trapped, bearing concentrated costs). The phonics_research_school (institutional power, constrained exit, excluded from authority) sits near d=0.60–0.70 (not full target but substantially constrained by the constraint's enforcement machinery). No directionality overrides are needed; the derivation chain produces the correct d values from beneficiary/victim + power + exit declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rigid, meaningless phonics drill) is DEAD in contemporary pedagogy — balanced-literacy and modern phonics approaches have solved it by integrating explicit decoding with authentic literature. Yet the whole-language constraint persists at high levels of suppression and extraction. This is textbook mandatrophy: the founding problem is gone but the institutional arrangement persists because teacher training, textbook adoption, and specialist authority are invested in the whole-language framework. The constraint should be classified tangled_rope (real coordination + real extraction) rather than rope (which would imply the extraction is justified by the coordination necessity). The theater_ratio rise from 0.28 to 0.41 tracks the shift from genuine pedagogy to institutional performance: more and more of the enforcement energy goes into defending the reading against contrary evidence rather than solving the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_vs_explicit_necessity,
    'Is reading-skill acquisition genuinely implicit (like language grammar) or does it require explicit instruction in the grapheme-phoneme mapping that writing systems instantiate?',
    'Randomized controlled trials comparing implicit-only exposure to explicit-phonics instruction for diverse reader populations, controlling for text difficulty, exposure time, and teacher quality. Replication across languages with different orthographic depths.',
    'If implicit exposure is sufficient for all readers, the whole-language reading is justified and the extraction is an unavoidable cost of coordination. If explicit instruction is necessary for some readers (e.g., dyslexic, low-literacy-background), then the constraint is extractive by design — it withholds necessary instruction from those who need it most.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_vs_explicit_necessity, empirical, 'Whether reading-skill acquisition requires explicit decoding instruction or emerges implicitly from exposure').

omega_variable(
    dyslexic_reading_requirements,
    'Do students with phonological processing deficits and rapid automatized naming weaknesses require explicit, structured phonics instruction to develop reading fluency, or can they acquire skills through implicit whole-language exposure?',
    'Longitudinal studies of reading trajectories for dyslexic students under whole-language vs. explicit-phonics instruction. Neuroimaging studies of reading-network activation in dyslexic readers learning implicitly vs. explicitly.',
    'If dyslexic readers require explicit phonics, the constraint is extractive toward this population by design — it denies them the instruction form their neurocognition requires. If dyslexic readers can acquire skills through exposure, the constraint''s harm to this population is incidental rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dyslexic_reading_requirements, empirical, 'Whether dyslexic readers can acquire decoding skills through implicit whole-language exposure').

omega_variable(
    institutional_gatekeeping_suppression,
    'Is the persistence of the whole-language reading maintained by genuine pedagogical evidence and genuine coordination benefits, or by institutional gatekeeping that suppresses contrary evidence and excludes alternative voices?',
    'Meta-analysis of reading-instruction research comparing effect sizes for implicit-exposure vs. explicit-phonics approaches. Analysis of citation patterns in reading-education journals, curriculum-committee membership, and teacher-training program structure. Interview study of reading specialists'' knowledge of phonics research and reasons for curriculum choices.',
    'If persistence is evidence-driven, the constraint is a transparent coordination arrangement with known trade-offs. If persistence is driven by institutional gatekeeping, the suppression metric is the active mechanism maintaining the reading despite contrary evidence — this would support reclassification toward snare or strengthen the tangled_rope reading by clarifying the enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_suppression, empirical, 'Whether the constraint''s persistence is evidence-driven or maintained by institutional gatekeeping').

omega_variable(
    reading_kernel_structure_ambiguity,
    'Does the reading_acquisition_mechanism kernel admit multiple structural readings (whole-language, phonics, balanced-literacy as genuinely distinct instantiations of one kernel), or does the disagreement reflect different empirical claims about the same mechanism rather than different readings of the same commitment?',
    'Close analysis of each reading''s core premises: does each claim to be answering ''how reading is acquired in nature'' (empirical claim, only one can be true) or ''what arrangement should govern reading instruction'' (normative claim, multiple readings possible)? If each is an empirical claim, they are competing hypotheses, not readings of a kernel. If each is a normative reading grounded in a commitment (e.g., to student agency, to evidence-based practice), they are readings of a kernel.',
    'If this is a kernel with multiple readings, the cs_structure axioms and reading_relations are correctly authored and the constraint is a normative reading. If this is a disputed empirical fact with only one true answer, the constraint is not a kernel reading but a false empirical claim, and the reclassification would be to ''false mountain'' or ''snare constructed to defend false empirical claim.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_structure_ambiguity, conceptual, 'Whether reading acquisition is a kernel that admits multiple normative readings or an empirical fact with one correct answer').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (active institutional exclusion of phonics research, gatekeeping of curriculum authority) or internalized (teachers and specialists who have been trained in the whole-language paradigm genuinely believe it is sufficient and do not recognize their belief as constrained)?',
    'Post-exit trajectory: if teachers or specialists who move away from the whole-language institutional context (e.g., private schools, different countries with different paradigms) maintain adherence to the reading, the suppression is partially internalized. If they shift to explicit-phonics practice when institutional gatekeeping is removed, suppression is primarily structural.',
    'If structural, the constraint''s suppression can be reduced through policy change (curriculum reform, institutional restructuring). If internalized, suppression persists even after institutional pressure is removed — the constraint has colonized the practitioners'' professional identity, making escape more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural institutional gatekeeping or internalized professional belief').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wlr_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(wlr_tr_t5, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(wlr_tr_t10, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(wlr_tr_t15, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(wlr_tr_t25, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(wlr_tr_t35, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(wlr_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(wlr_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(wlr_be_t5, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(wlr_be_t10, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(wlr_be_t15, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(wlr_be_t25, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(wlr_be_t35, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(wlr_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wlr_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(wlr_su_t5, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(wlr_su_t10, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(wlr_su_t15, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(wlr_su_t25, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(wlr_su_t35, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(wlr_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__whole_language_reading, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reading_acquisition_mechanism kernel. The phonics_reading and balanced_literacy_reading are sibling readings of the same kernel, not separate constraints. Each reading instantiates a different claim about how reading is acquired and what arrangement should govern instruction. All three stories should be linked via network.affects_constraints to reflect their mutual structural relationship: the whole-language reading influences the institutional conditions the phonics and balanced-literacy readings must operate within (curriculum authority, teacher training pipelines, textbook adoption). Decomposition follows DP-001 (ε-invariance): each reading has its own ε (whole-language: 0.68 extraction; phonics: lower extraction but potentially different victim set; balanced-literacy: attempting to balance extraction). Each reading is a distinct cs_structure with its own axioms, reference_frame, and drift_state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
