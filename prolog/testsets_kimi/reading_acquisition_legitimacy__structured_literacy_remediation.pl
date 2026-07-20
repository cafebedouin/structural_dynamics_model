% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Structured Literacy Remediation Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint story models the structured_literacy_remediation reading
 *   of the contested kernel reading_acquisition_legitimacy. The constraint
 *   asserts that reading instruction must be designed for the most vulnerable
 *   learners first, with legitimate instruction defined as explicit,
 *   cumulative, diagnostic, and multisensory. As a policy mechanism, it
 *   solves a genuine coordination problemâpreventing reading failure among
 *   at-risk populationsâbut enforces this solution through mandates that
 *   suppress alternative pedagogies, subordinate teacher autonomy, and
 *   channel public education budgets toward commercial structured-literacy
 *   curriculum vendors. The result is a tangled rope: real coordination for
 *   dyslexic and struggling readers, asymmetric extraction from educators and
 *   competing paradigms, and active enforcement by state agencies.
 *
 * KEY AGENTS:
 *   - structured_literacy_advocates (organized/mobile): Primary agenda-setters who define legitimate instruction and lobby for mandates
 *   - state_education_agencies (institutional/constrained): Enforce compliance through procurement and evaluation rules
 *   - structured_literacy_curriculum_vendors (powerful/arbitrage): Primary financial beneficiaries capturing mandated district spending
 *   - classroom_teachers (moderate/constrained): Primary targets paying through loss of autonomy and scripted compliance
 *   - balanced_literacy_educators (moderate/constrained): Excluded victims losing professional standing and certification
 *   - dyslexic_and_at_risk_readers (powerless/trapped): Nominal beneficiaries receiving intensive instruction but locked into the surveillance regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.75).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '9a08364c-9de0-46b5-b771-760e223bfc4b').
narrative_ontology:cs_kernel_codification('9a08364c-9de0-46b5-b771-760e223bfc4b', formalized).
narrative_ontology:cs_authority_grounding('9a08364c-9de0-46b5-b771-760e223bfc4b', expertise).
narrative_ontology:cs_interpretation_layer_present('9a08364c-9de0-46b5-b771-760e223bfc4b').
narrative_ontology:cs_reading_relation('9a08364c-9de0-46b5-b771-760e223bfc4b', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('9a08364c-9de0-46b5-b771-760e223bfc4b', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('9a08364c-9de0-46b5-b771-760e223bfc4b', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('9a08364c-9de0-46b5-b771-760e223bfc4b', foundational, vulnerable_learner_design_priority).
narrative_ontology:cs_axiom_status(vulnerable_learner_design_priority, holdable).
narrative_ontology:cs_axiom_grounding('9a08364c-9de0-46b5-b771-760e223bfc4b', vulnerable_learner_design_priority, empirically_contingent).
narrative_ontology:cs_axiom('9a08364c-9de0-46b5-b771-760e223bfc4b', foundational, universal_explicit_instruction_mandate).
narrative_ontology:cs_axiom_status(universal_explicit_instruction_mandate, holdable).
narrative_ontology:cs_axiom_grounding('9a08364c-9de0-46b5-b771-760e223bfc4b', universal_explicit_instruction_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('9a08364c-9de0-46b5-b771-760e223bfc4b', explicit_diagnostic_instruction_baseline).
narrative_ontology:cs_drift_state('9a08364c-9de0-46b5-b771-760e223bfc4b', contemporary_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a08364c-9de0-46b5-b771-760e223bfc4b', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexic_and_at_risk_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_vendors).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_educators).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, explicit_instruction_efficacy).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexia_prevention_through_early_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are the stated priority of the constraint; receive intensive explicit, multisensory, diagnostic instruction. Cannot opt out of the mandated instructional model or its accompanying assessment and progress-monitoring regime; their reading acquisition is heavily categorized and surveilled.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexic_and_at_risk_readers, beneficiary,
    powerless, biographical, trapped, national).

% Sell scope-and-sequence curricula, diagnostic screeners, and training programs that comply with the mandated instructional principles. Revenue grows as states adopt structured literacy mandates and require district purchases of approved materials.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_vendors, beneficiary,
    powerful, generational, arbitrage, national).

% Must abandon autonomous instructional decision-making and follow explicit, cumulative scripts and pacing guides. Administer frequent diagnostic probes and progress-monitor all students. Bear the professional and cognitive cost of rigid compliance; their expertise is subordinated to the mandated framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Trained in constructivist and balanced literacy methods, they are excluded from policy design and delegitimized by the new mandate. Lose access to professional development funding, face recertification requirements in structured literacy, and see their pedagogical expertise dismissed as unscientific. Many leave the profession or convert under duress.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_educators, excluded,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_educators, payer).

% Parent advocacy groups and professional organizations that lobbied for the mandate, define what counts as explicit and diagnostic, and train the trainers. They set the policy agenda and benefit from expanded cultural authority and institutional influence.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates, agenda_setter,
    organized, generational, mobile, national).

% Translate the structured literacy principles into procurement rules, teacher evaluation rubrics, and district audit protocols. They enforce compliance through funding conditions and accreditation. Bear political accountability for reading outcomes but lack capacity to evaluate vendor claims independently.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, state_education_agencies, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_curriculum_vendors).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__structured_literacy_remediation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing reading failure among at-risk and dyslexic students by standardizing explicit, systematic, and diagnostically responsive instructional methods across classrooms, districts, and states.
% TRANSFER_FUNCTION: Moves instructional authority, professional autonomy, and district procurement budgets from classroom teachers and holistic literacy programs to structured literacy curricula, diagnostic screeners, and vendor training ecosystems.
% ABSENT_VOICES: Balanced literacy and whole language researchers and practitioners are excluded from policy design; their empirical findings and practitioner knowledge are delegitimized as unscientific. Students who thrive under less rigid, literature-rich instruction are not represented in the mandate's design logic.
% DISAPPEARANCE_RATIONALE: Districts would revert to heterogeneous local practice, the commercial market for scripted curricula would contract sharply, teacher preparation would re-diversify, and the current surveillance and labeling regime for at-risk readers would dissolveâthough some districts might retain explicit instruction voluntarily.
% FOUNDING_PROBLEM: High rates of reading failure, particularly among dyslexic and disadvantaged students, in instructional environments that lacked systematic phonics, cumulative scope-and-sequence design, and early diagnostic intervention.
% FOUNDING_PROBLEM_CORROBORATION: Independent longitudinal studies and international literacy assessments confirm reading failure rates; however, the attribution of these failures solely to the absence of structured literacyâand the dismissal of other instructional variablesâis contested by literacy researchers outside the structured literacy advocacy network, and no independent consensus corroborates the specific remediation reading as the sole valid response.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial transfer of instructional authority and district funds from teachers and public systems to commercial vendors and mandated assessment regimes. Suppression (0.75) is high because the constraint's persistence depends on decertifying balanced literacy, dismissing whole-language research, and mandating vendor-approved materials. Theater ratio (0.40) captures the growing gap between the genuine explicit-instruction reference frame and its performative implementation as scripted, box-checking compliance. Accessibility collapse (0.70) registers that once a state mandates structured literacy, alternative pedagogies become practically unavailable. Resistance (0.50) reflects ongoing teacher pushback and emerging research critiques.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (advocates, vendors, state agencies) experience the constraint as necessary equity-driven coordination that corrects prior pedagogical failure. The payer seats (teachers, balanced-literacy educators) experience the same structure as extractive professional dispossession. The engine derives this divergence from beneficiary/victim declarations combined with exit differentials: vendors arbitrage across states, while teachers are constrained by licensing and employment.
 *
 * DIRECTIONALITY LOGIC:
 *   Dyslexic and at-risk readers are declared beneficiaries with trapped exit (compulsory schooling), yielding low directionalityâthey are subsidized by the constraint even while surveilled. Curriculum vendors are beneficiaries with arbitrage-grade exit, yielding very low directionality. Classroom teachers and balanced-literacy educators are victims with constrained exit, yielding high directionality near the full-target end. State agencies are agenda-setters with constrained exit, placing them at moderate directionality despite their enforcement role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâreading failure among at-risk students under prior regimesâremains live, so the constraint has not undergone mandatrophy. The tangled-rope classification prevents mislabeling the constraint as pure extraction (it does coordinate genuine reading acquisition for vulnerable learners) and prevents mislabeling it as pure coordination (it actively suppresses alternatives and extracts professional autonomy and public funds). If the founding problem were to resolve while the constraint persisted purely for vendor revenue and ideological lock-in, it would degrade toward piton or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_ambiguity,
    'Does the empirical evidence actually support the structured literacy reading over its sibling readings (balanced literacy, whole language), or do all readings cherry-pick from underdetermined cognitive science?',
    'Meta-analysis adjudicated by researchers with no financial or ideological stake in any reading, comparing effect sizes across instructional frameworks for heterogeneous learner populations.',
    'If the evidence is underdetermined, the kernel is driven by conceptual/preference commitments rather than empirical constraint, and the extraction is better understood as enforcement of a contested paradigm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_ambiguity, conceptual, 'Whether the kernel is empirically decidable or framed by preference').

omega_variable(
    commercial_capture_vs_coordination,
    'Is the measured extraction primarily from commercial curriculum vendors capturing public funds, or from the genuine resource cost of intensive instruction for at-risk learners?',
    'Independent cost accounting of structured literacy implementation (materials, training, assessment platforms) versus outcome gains, compared to lower-cost explicit instruction alternatives.',
    'Would determine whether the constraint should be split into two storiesâone for the instructional method (genuine coordination) and one for the vendor-capture regime (extraction)âor whether the extraction is inseparable from the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_capture_vs_coordination, empirical, 'Whether extraction is commercial capture or necessary coordination cost').

omega_variable(
    teacher_suppression_mechanism,
    'Is teacher compliance driven by structural enforcement (mandates, evaluations, funding threats) or by internalized belief in the science-of-reading framing?',
    'Interview and survey data from teachers in mandate states versus non-mandate states, tracking whether pedagogical beliefs shifted before or after policy adoption.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâteachers carry the constraint with them even if external enforcement relaxes, changing the directionality and extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.2).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.28).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 15, 0.35).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.42).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 25, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint instantiates the structured_literacy_remediation reading of kernel reading_acquisition_legitimacy. Sibling readings (phonics_decoding_primacy, whole_language_meaning_primacy, balanced_literacy_integration) are authored as separate constraints per the epsilon-invariance and committer-frame rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
