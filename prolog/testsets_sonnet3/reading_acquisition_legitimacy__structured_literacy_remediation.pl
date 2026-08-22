% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy / Dyslexia-First Remediation Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This story instantiates the structured-literacy-remediation reading of
 *   the reading-acquisition-legitimacy kernel: the claim that legitimate
 *   reading instruction must be designed from the vulnerable-learner end of
 *   the distribution outward, using explicit, cumulative, diagnostic,
 *   multisensory principles derived from dyslexia intervention research,
 *   applied preventatively to all students rather than as a remedial tier.
 *   Over roughly two decades (represented here as t=0 to t=20), this reading
 *   moved from a specialist remediation framework used for identified
 *   dyslexic students into statute in numerous US states as 'science of
 *   reading' legislation, mandating structured-literacy curricula, teacher
 *   retraining, and continuous diagnostic screening for entire elementary
 *   populations. The coordination function is genuine and well-evidenced for
 *   the at-risk subpopulation; the extraction grows as the reading's scope
 *   widens from targeted remediation to universal mandate, pulling budget
 *   toward credentialing bodies and curriculum vendors and imposing
 *   retraining and pacing costs on teachers and students for whom the
 *   intervention-grade design was never intended.
 *
 * KEY AGENTS:
 *   - students_with_reading_disabilities: primary intended beneficiary, powerless, trapped in whatever instructional model their district adopts
 *   - classroom_teachers_untrained_in_structured_literacy: bears retraining and compliance cost
 *   - school_districts_with_limited_budgets: bears procurement and infrastructure cost disproportionate to tax base
 *   - structured_literacy_credentialing_bodies: agenda-setter, lobbies for statutory codification of its own certification standard
 *   - orton_gillingham_curriculum_publishers: beneficiary, revenue scales with legislative mandate adoption
 *   - advanced_readers_subjected_to_universal_intervention_pacing: unintended payer, mismatched to universal design
 *   - bilingual_and_multilingual_learners_assessed_on_monolingual_diagnostic_norms: unintended payer, diagnostic misclassification risk
 *   - whole_language_and_balanced_literacy_advocates: excluded from legislative drafting once science-of-reading coalition won momentum
 *   - cognitive_science_reading_researchers: analytical observer, corroborates targeted claim more strongly than universal claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.38).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.55).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy / Dyslexia-First Remediation Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '35c156bc-45d1-448b-ad9e-0bb6ae574d79').
narrative_ontology:cs_kernel_codification('35c156bc-45d1-448b-ad9e-0bb6ae574d79', distributed).
narrative_ontology:cs_authority_grounding('35c156bc-45d1-448b-ad9e-0bb6ae574d79', expertise).
narrative_ontology:cs_interpretation_layer_present('35c156bc-45d1-448b-ad9e-0bb6ae574d79').
narrative_ontology:cs_reading_relation('35c156bc-45d1-448b-ad9e-0bb6ae574d79', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('35c156bc-45d1-448b-ad9e-0bb6ae574d79', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('35c156bc-45d1-448b-ad9e-0bb6ae574d79', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('35c156bc-45d1-448b-ad9e-0bb6ae574d79', foundational, instructional_design_must_prioritize_most_vulnerable_learner_profile).
narrative_ontology:cs_axiom_status(instructional_design_must_prioritize_most_vulnerable_learner_profile, holdable).
narrative_ontology:cs_axiom_grounding('35c156bc-45d1-448b-ad9e-0bb6ae574d79', instructional_design_must_prioritize_most_vulnerable_learner_profile, empirically_contingent).
narrative_ontology:cs_axiom('35c156bc-45d1-448b-ad9e-0bb6ae574d79', foundational, universal_preventative_intervention_grade_instruction_is_warranted_for_all_learners).
narrative_ontology:cs_axiom_status(universal_preventative_intervention_grade_instruction_is_warranted_for_all_learners, holdable).
narrative_ontology:cs_axiom_grounding('35c156bc-45d1-448b-ad9e-0bb6ae574d79', universal_preventative_intervention_grade_instruction_is_warranted_for_all_learners, instrumental).
narrative_ontology:cs_axiom('35c156bc-45d1-448b-ad9e-0bb6ae574d79', secondary, decoding_emergence_without_explicit_instruction_is_unreliable_for_at_risk_readers).
narrative_ontology:cs_axiom_status(decoding_emergence_without_explicit_instruction_is_unreliable_for_at_risk_readers, holdable).
narrative_ontology:cs_axiom_grounding('35c156bc-45d1-448b-ad9e-0bb6ae574d79', decoding_emergence_without_explicit_instruction_is_unreliable_for_at_risk_readers, empirically_contingent).
narrative_ontology:cs_reference_frame('35c156bc-45d1-448b-ad9e-0bb6ae574d79', targeted_dyslexia_remediation_practice).
narrative_ontology:cs_drift_state('35c156bc-45d1-448b-ad9e-0bb6ae574d79', post_science_of_reading_legislative_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('35c156bc-45d1-448b-ad9e-0bb6ae574d79', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_reading_disabilities).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_credentialing_bodies).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, orton_gillingham_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_diagnostic_industry).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers_untrained_in_structured_literacy).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_limited_budgets).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, advanced_readers_subjected_to_universal_intervention_pacing).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, bilingual_and_multilingual_learners_assessed_on_monolingual_diagnostic_norms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically failed by meaning-first or balanced approaches that did not provide explicit, cumulative, multisensory phonics instruction; under this reading they receive structured diagnostic screening and Orton-Gillingham-style remediation from the outset. They have no capacity to select their own instructional model and depend entirely on district adoption of the mandate.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_reading_disabilities, beneficiary,
    powerless, biographical, trapped, national).

% Trained under prior teacher-preparation paradigms (often whole-language or balanced-literacy coursework) and now required to retrain, requalify, and restructure lesson plans to comply with structured-literacy mandates and diagnostic reporting requirements, often without paid time or adequate district support. Exit means leaving the profession or the jurisdiction; remaining means absorbing the retraining cost personally.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers_untrained_in_structured_literacy, payer,
    moderate, biographical, constrained, national).

% Must purchase licensed structured-literacy curricula, fund continuous diagnostic assessment infrastructure, and retrain entire elementary staffs to comply with state mandates modeled on this reading. Poorer districts face compliance costs disproportionate to their tax base and risk state sanctions or funding clawbacks for noncompliance.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_limited_budgets, payer,
    moderate, generational, constrained, regional).

% Certify teachers and schools in Orton-Gillingham-derived methodologies, lobby state legislatures to write specific certification requirements into law, and control the credentialing pipeline that districts must purchase compliance through. They set the diagnostic and instructional standard that gets codified into statute.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_credentialing_bodies, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_credentialing_bodies, beneficiary).

% Sell licensed scripted curricula, diagnostic assessment batteries, and mandated professional-development packages to districts required by law to adopt structured-literacy programs. Revenue scales directly with the number of jurisdictions that codify this reading into procurement requirements.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, orton_gillingham_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Already decode fluently and would benefit from meaning-rich, accelerated instruction, but under universal-preventative structured-literacy mandates are placed through the same explicit cumulative sequence and diagnostic checkpoints designed for at-risk readers, slowing engagement and potentially disengaging capable readers from independent reading.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, advanced_readers_subjected_to_universal_intervention_pacing, payer,
    powerless, biographical, trapped, local).

% Screened with phonological-awareness diagnostics normed on monolingual English speakers; cross-linguistic transfer effects and code-switching patterns are frequently misread as decoding deficits, routing these students into intensive remediation tracks calibrated for a different underlying profile.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, bilingual_and_multilingual_learners_assessed_on_monolingual_diagnostic_norms, payer,
    powerless, biographical, trapped, national).

% Argue that universal intervention-grade instruction is unnecessary overkill for the majority of learners who acquire literacy adequately through balanced or meaning-rich approaches, and that legislative codification of one reading forecloses professional judgment and curricular pluralism. Largely shut out of state literacy-law drafting processes once the science-of-reading coalition achieved legislative wins.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_and_balanced_literacy_advocates, excluded,
    organized, generational, constrained, national).

% Study reading acquisition across populations and evaluate the evidence base underlying specific instructional claims; some corroborate structured literacy's efficacy for at-risk readers specifically while noting the evidence is thinner for universal preventative application to all learners regardless of risk profile.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, cognitive_science_reading_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__structured_literacy_remediation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of children with dyslexia and reading disabilities being systematically failed by discovery-based or meaning-first instruction that never makes the alphabetic code explicit — structured, cumulative, diagnostic instruction reliably improves outcomes for this population and reduces the population of students who reach late elementary school unable to decode.
% TRANSFER_FUNCTION: Moves instructional authority, curricular budget, and teacher-training investment from generalist elementary pedagogy and existing literacy coursework toward credentialed structured-literacy specialists, licensed curriculum publishers, and diagnostic-assessment vendors; moves compliance burden and retraining cost onto incumbent teachers and underfunded districts.
% ABSENT_VOICES: Whole-language and balanced-literacy advocates, bilingual education specialists concerned about diagnostic norming, and teachers who successfully taught reading under prior paradigms are largely absent from the state legislative drafting sessions that codify structured-literacy mandates into law, once the science-of-reading coalition secured legislative momentum.
% DISAPPEARANCE_RATIONALE: Advocates for at-risk readers say the world would rearrange badly — dyslexic and struggling readers would again be routed through discovery-based instruction that has demonstrably failed a subset of learners for decades, so the mandate's disappearance would restore documented harm. Critics of universal application say the underlying diagnostic and remediation capacity for at-risk students could persist as a targeted intervention tier without the universal preventative mandate, meaning most of the apparatus (statewide procurement, universal pacing, credentialing requirements) is separable from the core coordination function and its removal would leave most classrooms largely unchanged.
% FOUNDING_PROBLEM: A persistent, well-documented population of children — historically 5-15% of learners, disproportionately without home literacy support or private tutoring resources — was not acquiring functional decoding skills under whole-language and even many balanced-literacy programs, producing lifelong literacy deficits, especially among low-income and minority students without private remediation resources.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science researchers outside the credentialing and publishing industries corroborate that the founding problem (systematic failure of discovery-based instruction for a subset of at-risk readers) was real and remains real for that subset. The same independent research base is more equivocal about whether the problem justifies universal preventative structured-literacy instruction for all learners rather than targeted, tiered intervention — a distinction the legislative mandates as enacted frequently do not preserve.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, contested).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.38 at t=20, rising from 0.18) because the coordination function for the targeted population is real and well-evidenced, but the reading's structural delta -- 'all students receive intervention-grade instruction preventatively' -- extends a remediation-grade intervention to a universal population, creating rent capture by credentialing and publishing interests as more jurisdictions codify the mandate into procurement law. Suppression is authored moderate-high (0.55) and rising (from 0.30) because compliance is increasingly backed by state statute, teacher-licensure requirements, and district funding conditions rather than persuasion alone -- the enforcement trajectory reflects the science-of-reading legislative wave hardening from advisory guidance into binding mandate over the interval. Theater ratio is kept low-moderate (0.22) because the diagnostic and instructional apparatus is substantially functional for its intended population, not merely performative; it rises modestly as universal screening produces increasing volumes of assessment activity whose marginal diagnostic value for already-fluent readers is low.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialing-body and publisher seats, this reading is coordination succeeding exactly as designed: measurable outcome improvement for at-risk readers, statutory adoption as proof of legitimacy. From the seat of an untrained veteran teacher or an underfunded rural district, the same statute reads as unfunded mandate: forced retraining, purchased curricula, and diagnostic overhead imposed regardless of local student population's actual risk profile. From the seat of an already-fluent or multilingual reader, the universal design applies an intervention calibrated for a different population's needs, producing engagement or misclassification costs with no corresponding benefit. The engine should compute these seats as classifying the same structural facts differently -- payer seats trending toward tangled-rope-as-experienced-snare, beneficiary/agenda-setter seats trending toward rope-as-experienced-tangled-rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with reading disabilities and credentialing/publishing interests sit near the beneficiary end: the constraint subsidizes them directly (better outcomes for the former, captured procurement revenue for the latter). Untrained teachers and underfunded districts sit near the target end: they bear compliance and retraining cost with constrained exit (leaving the profession or jurisdiction is costly). Advanced readers and multilingual learners are targets of a narrower, unintended kind -- the universal design was not built with their profile in mind, and their exit options are trapped (a child cannot select their own school's curriculum). Excluded whole-language advocates are neither beneficiaries nor targets in the transactional sense but are shut out of the venue where the mandate's scope is set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem -- discovery-based instruction failing a documented at-risk population -- remains genuinely live and is corroborated by researchers outside the credentialing/publishing beneficiary set; this is not a case of an obsolete mandate persisting by inertia. What is contested is scope: whether solving that live problem requires universal preventative application (this reading's structural delta) or a targeted remediation tier (which would look structurally different and less extractive). Classifying this as tangled_rope rather than snare or rope preserves that the coordination function is real and non-trivial for its intended population while still registering that the mandate, as codified, extracts from populations the founding problem was never about -- avoiding both the error of dismissing structured literacy as pure rent-seeking and the error of treating universal, unscoped application as costless coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeted_versus_universal_scope_ambiguity,
    'Does the well-evidenced coordination benefit for at-risk readers justify universal preventative application of intervention-grade instruction to the entire student population, or does universal application constitute extraction beyond what the founding problem warrants?',
    'Comparative outcome studies contrasting universal structured-literacy mandates against tiered/targeted-remediation models with equivalent screening but non-universal intervention intensity, tracking outcomes for at-risk, average, and advanced readers separately, plus cost-per-outcome-improvement analysis by tier.',
    'If tiered targeting produces equivalent outcomes for at-risk readers at substantially lower cost and without pacing costs to advanced/fluent readers, this reading''s universal-scope structural delta would be revealed as largely extractive overhead riding on a genuine but narrower coordination core -- pushing the classification toward snare-adjacent. If universal application produces meaningfully better population-wide outcomes (e.g., by catching borderline cases tiered screening misses), the tangled_rope classification with its acknowledged coordination function is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeted_versus_universal_scope_ambiguity, empirical, 'Whether universal preventative scope is warranted by the evidence or is extraction beyond the founding problem''s boundary.').

omega_variable(
    credentialing_capture_of_legislative_process,
    'To what extent did structured-literacy credentialing bodies and curriculum publishers shape the specific statutory language of state science-of-reading mandates (e.g., naming specific proprietary methodologies or requiring specific vendor certifications) versus the legislation reflecting independent research-community consensus?',
    'Legislative history analysis: lobbying disclosure records, bill drafting correspondence, and comparison of statutory curriculum-approval lists against campaign contribution and consulting-fee records of credentialing/publishing entities named as approved vendors.',
    'High capture would indicate the beneficiary declarations for credentialing_bodies and curriculum_publishers reflect not incidental benefit from good policy but active rent-seeking that shaped the mandate''s scope and specificity -- supporting a harder read toward tangled_rope with a stronger extraction component than currently authored (0.38).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialing_capture_of_legislative_process, empirical, 'Whether legislative codification reflects independent research consensus or vendor lobbying capture.').

omega_variable(
    multilingual_diagnostic_validity,
    'Are the phonological-awareness and decoding diagnostics used for universal screening under this reading valid and reliable for bilingual and multilingual learners, or do they systematically misclassify cross-linguistic transfer patterns as decoding deficits?',
    'Psychometric validation studies of the specific diagnostic instruments used in structured-literacy screening batteries against populations of bilingual learners with independently verified decoding proficiency in at least one language, examining false-positive rates for intervention-track placement.',
    'High misclassification rates would substantially increase the victim standing of bilingual_and_multilingual_learners and would suggest the diagnostic apparatus, while functional for its designed population, generates a real and previously under-acknowledged extraction on a distinct subpopulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilingual_diagnostic_validity, empirical, 'Whether universal diagnostic screening is validly calibrated across linguistic populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 4, 0.13).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 8, 0.16).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 12, 0.18).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 16, 0.2).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(read_be_t4, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 8, 0.29).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(read_su_t4, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the reading_acquisition_legitimacy kernel. Each reading instantiates a structurally distinct constraint with its own epsilon: phonics_decoding_primacy (narrower coordination scope, likely lower extraction, closer to rope), whole_language_meaning_primacy (contested coordination function itself, given documented failure for at-risk readers -- likely higher contested extraction), balanced_literacy_integration (moderate structure, likely intermediate epsilon), and this reading, structured_literacy_remediation (highest structure and universal-preventative scope, moderate-rising extraction driven by the tension between well-evidenced targeted benefit and less-evidenced universal application). All four share the underlying kernel dispute over what counts as legitimate reading instruction and should be read as a family, not as observer-relative measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
