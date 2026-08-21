% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Systematic Phonics Instruction for Reading Acquisition
 *   domain: Educational Psychology/Literacy Pedagogy/Cognitive Science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics_reading' of the
 *   'reading_acquisition_mechanism' kernel, asserting that reading
 *   acquisition requires explicit, systematic instruction in grapheme-phoneme
 *   correspondence as a foundational skill. It is a pedagogical approach
 *   grounded in cognitive science, which has gained significant traction in
 *   educational policy. The constraint operates as a Tangled Rope, providing
 *   genuine coordination (effective reading instruction) but also involving
 *   asymmetric extraction (teacher autonomy, initial student effort) and
 *   requiring active enforcement through curriculum mandates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.68).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.75).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Systematic Phonics Instruction for Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "Educational Psychology/Literacy Pedagogy/Cognitive Science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6').
narrative_ontology:cs_kernel_codification('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', formalized).
narrative_ontology:cs_authority_grounding('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', expertise).
narrative_ontology:cs_interpretation_layer_present('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6').
narrative_ontology:cs_reading_relation('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', foundational, grapheme_phoneme_correspondence_is_primary).
narrative_ontology:cs_axiom_status(grapheme_phoneme_correspondence_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', grapheme_phoneme_correspondence_is_primary, empirically_contingent).
narrative_ontology:cs_axiom('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', foundational, explicit_instruction_is_optimal).
narrative_ontology:cs_axiom_status(explicit_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', explicit_instruction_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', evidence_based_pedagogical_consensus).
narrative_ontology:cs_drift_state('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', contemporary_educational_policy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6bd8ff1e-ac51-4ce2-8550-bc6a1824eaf6', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, literacy_researchers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, parents).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, science_of_reading_principles).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, cognitive_load_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and produce the evidence base supporting systematic phonics. They benefit from the adoption of their research findings into policy and practice, seeing it as a vindication of scientific principles in education.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, literacy_researchers, agenda_setter,
    institutional, generational, analytical, global).

% Mandate curriculum frameworks and teacher training programs that prioritize systematic phonics. They bear the political cost of pedagogical debates but benefit from improved literacy rates and alignment with scientific consensus.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, education_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Are required to implement systematic phonics curricula, often involving significant retraining and adherence to prescribed methods. This narrows their pedagogical discretion and increases initial instructional effort, though it can lead to more consistent student outcomes.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teachers, payer,
    moderate, biographical, constrained, local).

% Undergo explicit, structured instruction in phonics, which can be cognitively demanding initially. While it provides foundational skills, it may reduce exposure to broader literature in early stages compared to other methods.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, students, payer,
    powerless, biographical, trapped, local).

% Benefit disproportionately from the explicit and systematic nature of phonics instruction, which provides them with the decoding skills necessary to become fluent readers, often overcoming initial difficulties that other methods fail to address.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, local).

% Are proponents of pedagogical approaches that emphasize implicit learning through meaningful text engagement, often viewing explicit phonics as unnatural or detrimental. Their methods are de-emphasized or excluded by the dominance of systematic phonics policies.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_advocates, excluded,
    organized, biographical, constrained, national).

% Benefit from their children receiving evidence-based instruction that leads to stronger reading outcomes. They may also bear costs if they seek supplemental tutoring for alternative approaches or if they disagree with the pedagogical method.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, parents, beneficiary,
    moderate, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To standardize and ensure the delivery of effective, evidence-based reading instruction, particularly for foundational decoding skills, across diverse educational settings.
% TRANSFER_FUNCTION: Transfers pedagogical authority from individual teacher discretion to scientifically validated curricula and methods; transfers initial cognitive effort from implicit discovery to explicit, structured learning for students.
% ABSENT_VOICES: Advocates for whole language or purely balanced literacy approaches are largely excluded from policy-making and curriculum design, as their core tenets are seen as incompatible with the phonics-first emphasis. They would argue for more holistic, less prescriptive methods.
% DISAPPEARANCE_RATIONALE: If systematic phonics instruction vanished overnight, reading outcomes, especially for struggling learners, would likely become more variable and less predictable. The 'reading wars' debate would intensify without a clear evidence-based anchor, leading to pedagogical fragmentation and potential decline in overall literacy rates.
% FOUNDING_PROBLEM: Persistent high rates of reading failure, particularly among students from disadvantaged backgrounds, and a lack of consistent, effective instructional methods across schools.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science research, longitudinal studies on reading development, and educational psychologists widely corroborate the ongoing problem of reading failure and the efficacy of systematic phonics as a solution. This is attested by numerous independent academic bodies and government reports.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high due to the significant initial instructional cost for students and the imposition on teacher autonomy, requiring adherence to specific, often rigid, curricula. Suppression (0.75) is also high because this approach actively de-emphasizes or excludes alternative pedagogical methods, particularly whole language. The theater ratio (0.15) is low, reflecting that the constraint is highly functional and directly aims to achieve its stated goal of improving reading outcomes, with minimal performative maintenance. Accessibility collapse (0.80) is high for alternative pedagogical approaches for teachers, as the systematic phonics framework becomes the dominant, often mandated, method. Resistance (0.60) is moderate-high, as proponents of other methods continue to contest its dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of literacy researchers and policymakers, this constraint is a necessary, evidence-based coordination mechanism to ensure effective reading instruction. From the perspective of many teachers, it can be experienced as an extractive imposition that limits their professional judgment and requires significant effort to implement, while some students may find the initial explicit instruction challenging. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Literacy researchers and education policymakers act as agenda-setters and beneficiaries, as their evidence and policies are adopted. Struggling readers are primary beneficiaries, gaining essential decoding skills. Teachers and students are payers, bearing the costs of increased instructional effort and reduced pedagogical flexibility. Whole language advocates are excluded, as their methods are suppressed by this approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_autonomy_vs_fidelity,
    'Is the measured extraction from teachers (loss of autonomy) a necessary cost for instructional fidelity and student outcomes, or an unnecessary imposition?',
    'Comparative studies of teacher satisfaction and student outcomes in contexts with varying degrees of pedagogical prescription vs. autonomy, controlling for instructional quality.',
    'If unnecessary, the constraint''s effective extraction is higher than justified by coordination, suggesting a Snare-like element. If necessary, the extraction is a legitimate cost of effective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_autonomy_vs_fidelity, conceptual, 'Whether teacher autonomy is genuinely incompatible with effective phonics instruction.').

omega_variable(
    long_term_remediation_cost_reduction,
    'Does the high initial instructional cost of systematic phonics genuinely lead to a proportional reduction in long-term remediation costs for struggling readers?',
    'Longitudinal studies tracking cohorts of students through their educational careers, comparing remediation needs and costs for those exposed to systematic phonics versus other methods.',
    'If long-term costs are not significantly reduced, the initial extraction from students and teachers is less justified, increasing the overall effective extraction. If reduced, the coordination function is strongly vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_remediation_cost_reduction, empirical, 'Verification of the claimed long-term benefits offsetting initial costs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative pedagogical approaches structural (mandated curricula, policy) or internalized (teachers adopting due to perceived pressure or lack of alternatives)?',
    'Surveys and qualitative studies of teacher decision-making in contexts where mandates are relaxed or absent, observing the persistence of phonics-first approaches.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as teachers carry the suppression with them even without explicit mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for pedagogical choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(read_tr_t1995, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(read_be_t1995, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(read_su_t1995, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, teacher_training_curriculum).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, literacy_assessment_standards).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_mechanism' kernel. Its ε value differs significantly from sibling readings like 'whole_language_reading' due to its explicit, systematic nature and the associated costs and benefits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
