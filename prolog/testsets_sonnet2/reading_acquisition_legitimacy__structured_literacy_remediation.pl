% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__structured_literacy_remediation, []).

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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation Mandate (Dyslexia-First Reading Policy)
 *   domain: education_policy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the structured-literacy-remediation reading of
 *   the reading-acquisition-legitimacy kernel: legitimate reading instruction
 *   is defined by explicit, cumulative, diagnostic, multisensory
 *   structured-literacy principles, designed first for the most vulnerable
 *   decoders (dyslexic and at-risk readers) and then extended, via state
 *   'science of reading' mandates, to all K-3 students preventatively. The
 *   coordination function is genuine — decades of under-identification of
 *   phonological-processing deficits under looser instructional regimes is a
 *   real, well-documented problem this reading solves. But as state
 *   legislatures have converted a targeted remediation model into a universal
 *   instructional mandate, a coordination/extraction hybrid has emerged:
 *   curriculum vendors, certification bodies, and diagnostic-testing
 *   infrastructure now have an institutional incentive to expand the
 *   mandate's scope beyond the population it was designed to serve, and
 *   school districts and non-target students bear costs of universalization
 *   that the original remediation-first rationale does not by itself justify.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia_risk_markers: primary intended beneficiary (powerless/trapped) — the population the coordination function was built for
 *   - students_who_thrive_under_lighter_touch_instruction: unintended payer (powerless/trapped) — bears universalized intensity without matching need
 *   - classroom_teachers_retrained_under_mandate: payer (moderate/constrained) — bears retraining and fidelity-compliance cost
 *   - structured_literacy_curriculum_vendors and orton_gillingham_certification_bodies: organized beneficiaries with arbitrage exit — capture the expanding procurement and certification market
 *   - cognitive_science_researchers: analytical observer — supplies the evidence base but has no capture stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.38).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.52).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Mandate (Dyslexia-First Reading Policy)").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, 'c8590052-b167-4756-a216-979340c9a520').
narrative_ontology:cs_kernel_codification('c8590052-b167-4756-a216-979340c9a520', distributed).
narrative_ontology:cs_authority_grounding('c8590052-b167-4756-a216-979340c9a520', expertise).
narrative_ontology:cs_interpretation_layer_present('c8590052-b167-4756-a216-979340c9a520').
narrative_ontology:cs_reading_relation('c8590052-b167-4756-a216-979340c9a520', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('c8590052-b167-4756-a216-979340c9a520', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('c8590052-b167-4756-a216-979340c9a520', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_axiom('c8590052-b167-4756-a216-979340c9a520', foundational, vulnerable_learner_sequencing_primacy).
narrative_ontology:cs_axiom_status(vulnerable_learner_sequencing_primacy, holdable).
narrative_ontology:cs_axiom_grounding('c8590052-b167-4756-a216-979340c9a520', vulnerable_learner_sequencing_primacy, empirically_contingent).
narrative_ontology:cs_axiom('c8590052-b167-4756-a216-979340c9a520', secondary, universal_preventive_intervention_grade_instruction).
narrative_ontology:cs_axiom_status(universal_preventive_intervention_grade_instruction, holdable).
narrative_ontology:cs_axiom_grounding('c8590052-b167-4756-a216-979340c9a520', universal_preventive_intervention_grade_instruction, instrumental).
narrative_ontology:cs_reference_frame('c8590052-b167-4756-a216-979340c9a520', vulnerable_learner_diagnostic_primacy).
narrative_ontology:cs_drift_state('c8590052-b167-4756-a216-979340c9a520', post_universal_mandate_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8590052-b167-4756-a216-979340c9a520', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_risk_markers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, orton_gillingham_certification_bodies).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_diagnosticians).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers_retrained_under_mandate).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, students_who_thrive_under_lighter_touch_instruction).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_bearing_retraining_costs).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_curriculum_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot self-advocate for instructional method; depend entirely on the school system correctly diagnosing and remediating their decoding deficits early. Structured literacy's diagnostic-cumulative design is built around exactly their profile — without it, many fail to become fluent readers at all. They have no exit from whatever method their assigned classroom uses.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia_risk_markers, beneficiary,
    powerless, biographical, trapped, national).

% Acquire reading readily through exposure and lighter explicit instruction; forced into the same intensive, drill-heavy, diagnostic-testing-saturated sequence designed for struggling readers. Experience the mandate as time cost, reduced engagement with authentic texts, and testing fatigue disproportionate to their needs. Cannot opt into a different instructional track within a public classroom.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_who_thrive_under_lighter_touch_instruction, payer,
    powerless, biographical, trapped, national).

% Must undergo mandatory retraining and certification in structured literacy methods, often at personal expense of time and sometimes money, frequently discarding instructional practices built over a career. Job continuation is increasingly conditioned on demonstrated fidelity to the structured-literacy scope and sequence, regardless of their classroom's actual student mix. Exit means leaving the profession or the jurisdiction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers_retrained_under_mandate, payer,
    moderate, biographical, constrained, regional).

% Sell scripted structured-literacy curricula, diagnostic assessment suites, and certification programs; state mandates requiring 'evidence-based' or 'science of reading' aligned materials create a captive procurement market. Lobby state legislatures to codify structured literacy as the only legitimate approach, directly expanding their addressable market.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_vendors, beneficiary,
    organized, generational, arbitrage, national).

% Administer the credentialing pipeline that mandates now route teachers through; collect certification fees and control the definition of 'qualified' structured-literacy instruction. Their institutional authority to certify legitimacy expands with every new state mandate.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, orton_gillingham_certification_bodies, beneficiary,
    organized, generational, arbitrage, national).

% Administer the continuous diagnostic assessments the framework requires; mandates that make diagnostic screening universal and recurring expand their referral pipeline and institutional relevance. Also help set the district-level implementation agenda, translating state mandates into local screening and intervention schedules.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_diagnosticians, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_diagnosticians, agenda_setter).

% Bear the direct budgetary cost of curriculum replacement, teacher retraining, diagnostic assessment licenses, and compliance reporting mandated by state 'science of reading' legislation, often unfunded or underfunded. Cannot opt out once state law ties funding or accreditation to compliance.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_bearing_retraining_costs, payer,
    institutional, generational, constrained, regional).

% Previously dominant curriculum publishers whose materials are being legislatively excluded from state-approved lists as structured literacy is codified into law; face market exclusion and lost contracts as procurement rules shift, and must retool or exit the K-3 reading market.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_curriculum_publishers, payer,
    powerful, biographical, constrained, national).

% Study the empirical basis for phonological-processing deficits and explicit-instruction efficacy; produce the evidence base the mandate cites, but do not administer, fund, or profit from its implementation and are excluded from most legislative drafting rooms.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, cognitive_science_researchers, observer,
    analytical, generational, analytical, global).

% Advocated hardest for dyslexia-aware mandates, often after years of unrecognized struggle, but have no seat in curriculum procurement or certification-body governance once the mandate is codified; their advocacy triggered the policy but their voice on implementation details is structurally absent from state board proceedings.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, parents_of_struggling_readers, excluded,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a K-3 reading system around the diagnostic and instructional needs of the most vulnerable decoders (dyslexic and at-risk readers), ensuring that instruction is explicit, cumulative, and multisensory enough that no child with a phonological-processing deficit is left to 'catch up' through incidental exposure — solving the real problem that under looser methods, struggling decoders are disproportionately never identified until failure is severe.
% TRANSFER_FUNCTION: Moves instructional time, teacher retraining burden, and curriculum-procurement budgets away from previously dominant balanced/whole-language approaches and their publishers, toward structured-literacy curriculum vendors, Orton-Gillingham-style certification bodies, and diagnostic-testing infrastructure — funded by school districts and absorbed as time cost by all students, including those who did not need the intervention-grade approach.
% ABSENT_VOICES: Parents of struggling readers, whose advocacy (often through dyslexia-parent coalitions) drove the legislative mandates, have no institutional role in the certification-body or curriculum-procurement decisions that followed; fluent-reading students and their teachers, who experience the universal intervention-grade sequence as unnecessary intensity, are rarely surveyed on instructional fit once the mandate is law.
% DISAPPEARANCE_RATIONALE: Advocates for at-risk readers say the world would rearrange sharply and badly: without the mandate, phonological-deficit students would again go unidentified until failure, as happened under decades of whole-language and unstructured balanced-literacy dominance. Critics of universal application say the world would rearrange only for the subset who need it — for fluent-track students and for teachers freed from prescriptive fidelity mandates, removal would restore instructional flexibility with little cost. The dispute is exactly about scope: should the arrangement exist for the vulnerable-learner population it targets, or persist as a universal mandate for all learners.
% FOUNDING_PROBLEM: Decades of whole-language and loosely-structured balanced-literacy instruction left phonological-processing-deficit readers (including dyslexic students) undiagnosed and unremediated until reading failure was severe and harder to reverse; the field lacked a diagnostic-first, cumulative, explicit-instruction pipeline calibrated to the most vulnerable decoders.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists studying phonological processing (an analytical seat with no financial stake in curriculum sales) corroborate that the founding problem — undiagnosed decoding deficits under unstructured instruction — was real and remains live for a meaningful subset of learners. However, independent literacy-outcomes researchers and some school districts report that once codified as a universal mandate rather than a targeted intervention, the arrangement increasingly serves procurement and certification markets rather than tracking need, and that the diagnostic-first case for universal (not targeted) application is weaker than the case for its original remediation-first population.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, contested).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).
:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38 at interval end) because the coordination function for the target population is real and well-evidenced, but a genuine transfer exists once the mandate is applied universally: districts, non-target students, and displaced publishers bear costs that track legislative and procurement dynamics more than diagnosed need. Suppression is moderate (0.52) — teachers and displaced publishers face real constraint (certification requirements, procurement exclusion) but are not trapped the way the powerless student populations are; the coercive apparatus is real but not maximal. Theater ratio is kept low (0.22) because most of the diagnostic and instructional activity is functionally load-bearing for the target population, though a growing share of compliance reporting and certification renewal is administrative overhead rather than instructional substance — hence the rising trajectory across the measurement grid.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a dyslexia-risk student or advocacy parent, this reading is close to a rope: it fixed decades of neglect. From the seat of a fluent-track student's teacher forced through scripted intervention-grade drills for a class that doesn't need it, or a district absorbing unfunded retraining mandates, the same arrangement reads as extractive overreach riding on a legitimate original justification. The engine computes these as different seat-level types from the same structural data; the claimed_type (tangled_rope) is authored to hold both readings rather than resolve the tension in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries — the target population of at-risk readers, curriculum vendors, certification bodies, and diagnosticians — sit near the low-d end: the arrangement subsidizes their outcomes (readers) or their institutional position (vendors, certifiers). Victims — fluent-track students, retrained teachers, cost-bearing districts, and displaced publishers — sit near the high-d end: they bear cost without corresponding need or benefit. The trapped exit options for both powerless student populations (beneficiary and payer alike) reflect that neither group can choose their classroom's instructional method; what differs is whether the method fits their profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — undiagnosed phonological-processing deficits under unstructured instruction — remains genuinely live for the target population (status: contested, but corroborated as at least partially live by independent cognitive-science researchers). This blocks a pure-mandatrophy reading: the arrangement has not simply outlived its function. What the analysis flags instead is scope-creep mandatrophy — the coordination function's legitimate boundary (remediation-first, vulnerable-learner-targeted) has been extended by legislative mandate to a universal population where the same evidentiary justification does not straightforwardly apply, and that extension aligns suspiciously well with vendor and certification-body revenue expansion. Classifying this as tangled_rope rather than snare or rope preserves both truths: real coordination for the target population, real asymmetric extraction from the universalized non-target population and cost-bearing institutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeted_vs_universal_scope_justification,
    'Does the evidentiary case for structured literacy''s explicit, cumulative, diagnostic design justify its universal application to all K-3 students, or only to the at-risk/dyslexic population it was originally calibrated for?',
    'Comparative outcome studies of universal-structured-literacy-mandate states versus states that apply structured literacy only to diagnostically identified at-risk students, controlling for teacher retraining quality and curriculum fidelity.',
    'If universal application shows no outcome advantage over targeted application for fluent-track readers, the universalized portion of the mandate is extractive scope-creep riding on a genuine remediation-first coordination function, supporting a snare-leaning reading for the non-target population specifically. If universal application shows meaningful preventive benefit even for fluent-track readers, the tangled_rope classification would understate the coordination function and a rope-leaning reading becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeted_vs_universal_scope_justification, empirical, 'Whether structured literacy''s universal mandate is evidence-justified beyond its target remediation population.').

omega_variable(
    vendor_certification_capture_of_science_of_reading_legislation,
    'To what extent has ''science of reading'' legislative language been shaped by curriculum-vendor and certification-body lobbying versus independent cognitive-science consensus?',
    'Tracing legislative drafting history, lobbying disclosure records, and comparing state statutory language against independent (non-vendor-funded) cognitive-science literature reviews and consensus statements.',
    'High vendor/certifier influence on statutory specificity (e.g., mandating particular branded programs or certification pathways) would strengthen the tangled_rope reading and could support reclassifying vendor/certifier gains as concentrated capture rather than incidental beneficiary status. Low influence, with legislation tracking independent research consensus, would support treating vendor gains as an incidental byproduct of a genuinely evidence-driven mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_certification_capture_of_science_of_reading_legislation, empirical, 'Whether curriculum and certification industries shaped the mandate''s scope for revenue capture.').

omega_variable(
    kernel_framing_alternative_remediation_vs_universal_axis,
    'Is the structural distinction between this reading and its siblings best framed on the remediation-population-first axis (as declared), or would framing it instead as a pure instructional-method axis (structured literacy vs. phonics vs. whole language vs. balanced) collapse the vulnerability-first premise into a variant of phonics_decoding_primacy?',
    'Compare classification outcomes if this reading were re-authored solely on instructional-method criteria (explicit/systematic phonics content) without the vulnerable-learner-first sequencing and universal-preventive-application claims; check whether the beneficiary/victim structure and claimed_type would remain stable.',
    'If the vulnerable-learner-first framing is dropped, this reading collapses toward phonics_decoding_primacy with a much smaller victim set (no universalization cost, no scope-creep tension) and would likely classify closer to rope. Retaining the vulnerable-learner-first-then-universalized framing (as authored) is what generates the tangled_rope classification via the universalization transfer. The chosen framing follows the source material''s own emphasis on ''most vulnerable learners first'' and ''all students receive intervention-grade instruction preventatively'' as the defining, non-negotiable structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_remediation_vs_universal_axis, conceptual, 'Alternative framing of the kernel axis and its effect on classification, per the CS-framing under-determination guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 4, 0.1).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 8, 0.13).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 12, 0.16).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 16, 0.18).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.2).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(read_be_t4, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 12, 0.31).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 16, 0.34).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(read_su_t4, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the reading_acquisition_legitimacy kernel, each authored as a separate constraint with its own ε. structured_literacy_remediation (this story) has the highest suppression and coordination intensity of the four because it is the only reading that (a) sequences legitimacy around a vulnerable-learner-first diagnostic mandate and (b) extends intervention-grade instruction preventatively to the universal population, producing the tangled_rope scope-creep dynamic. phonics_decoding_primacy shares the explicit-instruction premise but lacks the vulnerability-first sequencing and universalization claim, and would be expected to classify closer to rope. whole_language_meaning_primacy inverts the core premise entirely (meaning-making primacy, decoding-emerges-naturally) and would carry a very different beneficiary/victim structure. balanced_literacy_integration attempts synthesis and is the most contested of the four on evidentiary grounds. All four are linked here per the ε-invariance decomposition principle; none averages or references the others' ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
