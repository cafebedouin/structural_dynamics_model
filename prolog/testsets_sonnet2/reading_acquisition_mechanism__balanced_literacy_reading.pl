% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Instruction Framework
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint instantiates the balanced-literacy reading of the
 *   reading-acquisition kernel: the claim that reading acquisition requires
 *   BOTH explicit phonics instruction AND authentic literature exposure,
 *   integrated in practice, rather than either component alone. The 'reading
 *   wars' produced this as an institutional compromise position between
 *   phonics-only and whole-language-only camps. The structural delta this
 *   reading carries relative to its siblings is implementation fidelity:
 *   because the framework does not specify a required phonics dosage or
 *   sequencing standard, and because the institutions that administer it
 *   (schools of education, publishers) are more invested in the
 *   literature-immersion and cueing-strategy components inherited from whole
 *   language, balanced literacy programs frequently under-deliver systematic
 *   phonics while retaining full literature exposure — collapsing toward
 *   whole-language outcomes despite the balanced-literacy branding. This
 *   story concerns the balanced-literacy arrangement as implemented and
 *   administered, not an idealized fully-faithful version of it.
 *
 * KEY AGENTS:
 *   - balanced_literacy_publishers: Primary beneficiary (institutional/arbitrage) — sells the branded curricula and PD
 *   - schools_of_education_faculty: Primary beneficiary (institutional/identity_locked) — professional and scholarly identity built on the framework
 *   - district_curriculum_administrators: Agenda-setter (institutional/constrained) — adopts and mandates, rarely audits fidelity
 *   - classroom_teachers: Intermediate payer (moderate/constrained) — implements without adequate phonics training
 *   - struggling_readers, dyslexic_students, low_income_district_students: Primary targets (powerless/trapped) — bear the cost of under-dosed phonics
 *   - reading_science_researchers: Analytical observer (analytical) — documents the fidelity gap
 *   - systematic_phonics_advocates: Excluded voice (organized/constrained) — kept off adoption committees
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.52).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.58).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Reading Instruction Framework").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, 'fd8b1063-01a9-4481-9401-3c433c4ee22b').
narrative_ontology:cs_kernel_codification('fd8b1063-01a9-4481-9401-3c433c4ee22b', distributed).
narrative_ontology:cs_authority_grounding('fd8b1063-01a9-4481-9401-3c433c4ee22b', practice).
narrative_ontology:cs_interpretation_layer_present('fd8b1063-01a9-4481-9401-3c433c4ee22b').
narrative_ontology:cs_reading_relation('fd8b1063-01a9-4481-9401-3c433c4ee22b', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd8b1063-01a9-4481-9401-3c433c4ee22b', reading_acquisition_mechanism__whole_language_reading, influences).
narrative_ontology:cs_axiom('fd8b1063-01a9-4481-9401-3c433c4ee22b', foundational, code_and_meaning_instruction_are_jointly_necessary).
narrative_ontology:cs_axiom_status(code_and_meaning_instruction_are_jointly_necessary, holdable).
narrative_ontology:cs_axiom_grounding('fd8b1063-01a9-4481-9401-3c433c4ee22b', code_and_meaning_instruction_are_jointly_necessary, empirically_contingent).
narrative_ontology:cs_axiom('fd8b1063-01a9-4481-9401-3c433c4ee22b', secondary, teacher_professional_judgment_should_calibrate_the_mix).
narrative_ontology:cs_axiom_status(teacher_professional_judgment_should_calibrate_the_mix, holdable).
narrative_ontology:cs_axiom_grounding('fd8b1063-01a9-4481-9401-3c433c4ee22b', teacher_professional_judgment_should_calibrate_the_mix, instrumental).
narrative_ontology:cs_reference_frame('fd8b1063-01a9-4481-9401-3c433c4ee22b', reading_wars_synthesis_compromise).
narrative_ontology:cs_drift_state('fd8b1063-01a9-4481-9401-3c433c4ee22b', post_science_of_reading_movement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd8b1063-01a9-4481-9401-3c433c4ee22b', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, schools_of_education_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, district_curriculum_administrators).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, low_income_district_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, instructional_pluralism_doctrine).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, teacher_professional_judgment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell integrated curricula, leveled-reader libraries, and professional development packages built around the balanced-literacy label. Revenue depends on districts adopting and re-adopting branded materials; they market the framework as evidence-synthesizing middle ground and lobby state adoption committees. If phonics-only mandates spread, their leveled-reader catalog loses relevance.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_publishers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_publishers, agenda_setter).

% Trained generations of teachers in whole-language-descended balanced-literacy methods; careers, tenure cases, and teacher-certification curricula are built on this pedagogy. Reversing course would require repudiating decades of published scholarship and retraining. Their professional identity is fused with the framework's legitimacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, schools_of_education_faculty, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, schools_of_education_faculty, agenda_setter).

% Select and mandate curricula, train teachers in implementation, and answer to school boards for reading outcomes. Balanced literacy is politically comfortable because it can be described as honoring both camps; low fidelity to the phonics component is rarely audited, letting administrators claim compliance without measuring actual decoding instruction quality.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, district_curriculum_administrators, agenda_setter,
    institutional, biographical, constrained, regional).

% Required to deliver integrated phonics-plus-literature instruction, often without adequate training in systematic phonics sequencing. Bear the burden of reconciling the philosophical directive with thirty children who need explicit decoding practice; blamed when outcomes are poor, but rarely given authority to depart from the mandated approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Depend on systematic, sufficiently dosed phonics instruction to acquire decoding skill; in classrooms where the balance tilts toward literature immersion and cueing strategies, they do not receive it. Cannot select their own curriculum or teacher; the cost of insufficient phonics compounds each year as text complexity rises.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Require the most systematic, explicit, and intensive phonics instruction of any subgroup; when the balanced approach in practice under-delivers structured phonics, these students fall furthest behind and are frequently misdiagnosed as having a distinct disability rather than as under-instructed. Families with resources exit to private tutoring or diagnosis-driven remediation; most cannot.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Attend districts with the least capacity to supplement inadequate phonics instruction with outside tutoring, diagnostic testing, or intensive intervention; the gap between the framework's stated design and its low-fidelity implementation lands hardest here because these families cannot buy exit.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, low_income_district_students, payer,
    powerless, generational, trapped, regional).

% Conduct converging cognitive-science and intervention studies on decoding acquisition; document that a genuinely integrated version could work but that field implementations chronically under-deliver phonics systematicity while retaining full literature immersion, producing whole-language outcomes under a balanced-literacy label.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, reading_science_researchers, observer,
    analytical, civilizational, analytical, global).

% Parent and clinician advocacy groups pushing for phonics-first mandates; frequently characterized by education faculty and publishers as reductive or outdated, and excluded from curriculum-adoption committees dominated by balanced-literacy-trained administrators and academics.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, systematic_phonics_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its intended form, integrates two genuinely necessary components of reading acquisition — explicit code instruction and rich meaning-focused text exposure — so that decoding skill and comprehension/motivation develop together rather than one being sacrificed for the other.
% TRANSFER_FUNCTION: Moves instructional time, teacher training investment, and curriculum-adoption budgets toward materials and pedagogies branded 'balanced,' while the systematic-phonics component is frequently under-dosed; the resulting learning-outcome cost is transferred onto students who most need explicit decoding instruction and least have alternative access to it.
% ABSENT_VOICES: Systematic phonics advocates, structured-literacy clinicians, and parents of dyslexic children are frequently excluded from curriculum-adoption panels dominated by schools-of-education faculty and administrators already invested in the balanced framework; their objection — that 'balance' in practice means insufficient phonics — rarely reaches the adoption decision.
% DISAPPEARANCE_RATIONALE: Publishers and education-school faculty would say the field reverts to fragmented, non-integrated instruction and loses hard-won recognition that literature engagement matters; reading-science researchers and phonics advocates would say the practical effect of removing the balanced-literacy label is that districts adopt structured-literacy programs with genuine systematic phonics, and struggling readers' outcomes improve — the parties dispute which world follows.
% FOUNDING_PROBLEM: The 1990s-2000s 'reading wars' pitted phonics-only and whole-language-only camps against each other; balanced literacy was proposed as a research-informed synthesis that would end the conflict by honoring evidence for both code-based and meaning-based instruction.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive-science researchers (e.g., National Reading Panel-descended synthesis work, NAEP outcome analyses) attest from outside the beneficiary set that the 'balance' claimed in curriculum branding is rarely realized in classroom-level phonics dosage and sequencing, and that where it is not realized, outcomes track whole-language predictions rather than true-balance predictions; publishers and schools of education, the beneficiaries, are the primary sources asserting the founding problem remains solved by the framework as implemented.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, contested).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) and theater_ratio (0.61, above the 0.5 Goodhart-drift threshold) reflect a framework whose stated coordination function — genuine integration of code and meaning instruction — is increasingly substituted by branded materials and PD certifications that signal 'balance' without measurable phonics systematicity. Suppression (0.58) is moderate: no one is coercively barred from teaching phonics, but the professional incentive and credentialing structure (schools of education, adoption committees) makes deviation costly for individual teachers and administrators. Accessibility_collapse is moderate (0.4) — structured-literacy alternatives exist and are gaining traction, so alternatives have not fully collapsed, distinguishing this from a mountain or an unchallengeable snare. Resistance is substantial (0.6) and rising, driven by phonics advocates, journalistic exposés (e.g., 'Sold a Story'), and state-level legislative mandates for structured literacy.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats (publishers, education faculty, administrators), balanced literacy is real, evidence-synthesizing coordination — it resolved a genuine false dichotomy in the reading wars. From the payer seats (struggling readers, dyslexic students, classroom teachers), the same arrangement operates as extraction: instructional time and trust are transferred into a framework whose phonics half is chronically under-delivered, and the cost lands on children who cannot get sufficient explicit instruction elsewhere. The engine should compute these as structurally different seat experiences from the same base facts, not reconcile them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers and education faculty are declared beneficiaries: they collect revenue, prestige, and career security from the framework's continued adoption, and their exit options are far better (arbitrage/identity-embedded institutional position) than any payer's. Struggling readers, dyslexic students, and low-income district students are declared victims: they are trapped (cannot select curriculum or teacher), powerless, and bear the compounding cost of insufficient phonics dosage. Classroom teachers occupy an intermediate position — payers of professional and moral cost, but not primary beneficiaries — reflected in moderate power and constrained rather than trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending the unproductive phonics-vs-whole-language conflict via evidence synthesis) was real and briefly live. Its status is contested, not simply dead: a genuinely faithful implementation could still solve it. What has happened instead is drift — the coordination function persists in name while implementation fidelity has degraded toward whole-language defaults, particularly regarding phonics dosage and sequencing. Classifying this as tangled_rope rather than snare preserves the fact that a real coordination function exists at the design level (the theoretical synthesis), while classifying it as tangled_rope rather than rope registers the asymmetric extraction that field-level under-implementation imposes on the least-resourced learners. A pure mountain or rope reading would erase the documented fidelity gap; a pure snare reading would erase the genuine cognitive-science basis for wanting both components.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    faithful_implementation_vs_structural_drift,
    'Is the extraction measured here intrinsic to the balanced-literacy design, or is it a contingent implementation failure that a sufficiently specified, audited version of the framework would not exhibit?',
    'Comparative study of high-fidelity balanced-literacy programs (with mandated phonics scope-and-sequence, dosage minimums, and outcome audits) against low-fidelity (''balanced-in-name'') programs, controlling for student population; if high-fidelity implementations close the outcome gap with structured literacy, the extraction is contingent on implementation, not intrinsic to the design.',
    'If contingent, this constraint''s tangled_rope classification reflects a correctable enforcement/audit failure rather than a structural design flaw — pointing toward scaffold-style remediation (mandated fidelity standards with a sunset once outcomes converge). If intrinsic, the coordination story is closer to cover for chronic under-delivery, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(faithful_implementation_vs_structural_drift, empirical, 'Whether measured extraction stems from design or from implementation fidelity failure.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Where exactly does ''balanced literacy as designed'' end and ''balanced literacy collapsing into whole language'' begin — is there a principled dosage/sequencing threshold, or is the boundary itself contested terrain between the balanced_literacy_reading and whole_language_reading?',
    'Establish a consensus minimum phonics-instruction-dosage and sequencing-systematicity threshold from converging cognitive-science literature (e.g., structured literacy standards bodies) and classify field implementations against it.',
    'If no principled threshold can be established, the balanced_literacy_reading may not be a structurally distinct constraint from whole_language_reading in practice — most real-world instances would be relabeled as de facto whole-language implementation, undermining the claim that balanced literacy is a genuine third position rather than whole language with better branding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether balanced literacy is a structurally distinct reading or a relabeling of whole-language practice.').

omega_variable(
    administering_institution_capture,
    'Are schools-of-education faculty and curriculum publishers merely incidental beneficiaries of an underspecified framework, or has the framework been actively shaped/maintained by these institutions to preserve their existing pedagogical and commercial investments?',
    'Historical/documentary analysis of curriculum-adoption committee composition, publisher lobbying records, and teacher-certification standard revisions over the interval to assess whether specification of phonics-fidelity standards was actively resisted by beneficiary institutions.',
    'If active resistance to fidelity standards is documented, this strengthens the requires_active_enforcement declaration and the tangled_rope classification''s asymmetric-extraction gate; if the underspecification is merely inertial, the constraint drifts closer to piton (degraded coordination sustained by institutional inertia rather than active defense).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administering_institution_capture, empirical, 'Whether administering institutions actively resist phonics-fidelity standards or merely inherit underspecification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 15, 0.53).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 25, 0.61).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 25, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__balanced_literacy_reading, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the reading_acquisition_mechanism kernel. phonics_reading claims systematic phonics is foundational and sufficient as the necessary mechanism; whole_language_reading claims decoding emerges implicitly from authentic text exposure; balanced_literacy_reading (this story) claims both are necessary in integrated practice. The three readings have different beneficiary/victim structures and different epsilon values: phonics_reading (not authored here) would show low extraction if implemented as designed since it has clearer fidelity metrics (letter-sound sequences are auditable); whole_language_reading would likely show the highest extraction from struggling readers given the near-complete absence of an explicit-decoding coordination function; balanced_literacy_reading sits between them, carrying real coordination intent but severe institutional fidelity drift toward the whole_language pole. Do not average these into one constraint — each reading's institutions, incentives, and victim sets differ structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
