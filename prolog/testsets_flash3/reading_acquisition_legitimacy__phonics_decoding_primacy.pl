% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics Decoding Primacy in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of reading
 *   acquisition legitimacy, asserting that reading is fundamentally decoding
 *   and that legitimate instruction explicitly teaches the alphabetic
 *   principle through systematic phonics. This approach emphasizes high
 *   structure, explicit sequencing, and decodable texts, with the teacher
 *   acting as a direct instructor. It is often associated with the 'Science
 *   of Reading' movement and aims to identify struggling readers early via
 *   decoding assessments. This reading is one of several competing
 *   interpretations of the 'reading_acquisition_legitimacy' kernel.
 *
 * KEY AGENTS:
 *   - phonics_curriculum_publishers: Primary beneficiary (organized/mobile)
 *   - literacy_researchers_science_of_reading: Primary beneficiary (institutional/analytical)
 *   - early_career_teachers: Beneficiary (moderate/constrained)
 *   - students_with_dyslexia: Beneficiary/Payer (powerless/trapped)
 *   - students_from_low_literacy_homes: Beneficiary (powerless/trapped)
 *   - teachers_trained_in_whole_language: Payer (moderate/constrained)
 *   - parents_advocating_for_phonics: Agenda-setter (organized/mobile)
 *   - education_policy_makers: Agenda-setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.45).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.6).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics Decoding Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '0831d8b9-88e7-4993-be10-ec89c6444240').
narrative_ontology:cs_kernel_codification('0831d8b9-88e7-4993-be10-ec89c6444240', formalized).
narrative_ontology:cs_authority_grounding('0831d8b9-88e7-4993-be10-ec89c6444240', expertise).
narrative_ontology:cs_interpretation_layer_present('0831d8b9-88e7-4993-be10-ec89c6444240').
narrative_ontology:cs_reading_relation('0831d8b9-88e7-4993-be10-ec89c6444240', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('0831d8b9-88e7-4993-be10-ec89c6444240', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('0831d8b9-88e7-4993-be10-ec89c6444240', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('0831d8b9-88e7-4993-be10-ec89c6444240', foundational, alphabetic_principle_explicit_instruction).
narrative_ontology:cs_axiom_status(alphabetic_principle_explicit_instruction, holdable).
narrative_ontology:cs_axiom_grounding('0831d8b9-88e7-4993-be10-ec89c6444240', alphabetic_principle_explicit_instruction, empirically_contingent).
narrative_ontology:cs_axiom('0831d8b9-88e7-4993-be10-ec89c6444240', foundational, decoding_precedes_comprehension).
narrative_ontology:cs_axiom_status(decoding_precedes_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('0831d8b9-88e7-4993-be10-ec89c6444240', decoding_precedes_comprehension, empirically_contingent).
narrative_ontology:cs_reference_frame('0831d8b9-88e7-4993-be10-ec89c6444240', science_of_reading_consensus).
narrative_ontology:cs_drift_state('0831d8b9-88e7-4993-be10-ec89c6444240', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0831d8b9-88e7-4993-be10-ec89c6444240', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, literacy_researchers_science_of_reading).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, early_career_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_dyslexia).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, students_from_low_literacy_homes).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_whole_language).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, students_from_low_literacy_homes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from increased demand for systematic phonics programs and decodable texts. Their products align directly with this pedagogical approach, leading to higher sales and market influence.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Their research findings, which emphasize the importance of explicit phonics, are validated and promoted by this reading. They gain funding, influence, and academic recognition.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, literacy_researchers_science_of_reading, beneficiary,
    institutional, generational, analytical, global).

% Benefit from clear, structured instructional guidelines and readily available materials. This approach provides a concrete framework for teaching reading, reducing ambiguity and preparation time, especially for those with less experience.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, early_career_teachers, beneficiary,
    moderate, biographical, constrained, local).

% Are direct beneficiaries of explicit, systematic phonics instruction, which is often essential for them to acquire reading skills. However, they may also bear the cost of remediation if initial instruction is insufficient or if the curriculum is too rigid.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_dyslexia, payer).

% Benefit from explicit instruction in foundational reading skills that they may not acquire through informal exposure at home. This approach aims to level the playing field for them.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_from_low_literacy_homes, beneficiary,
    powerless, biographical, trapped, local).

% Face pressure to abandon prior training and adopt new methods, often requiring retraining and a shift in pedagogical philosophy. This can lead to professional discomfort and a sense of de-skilling.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_whole_language, payer,
    moderate, biographical, constrained, local).

% Actively lobby for the adoption of phonics-first approaches, often driven by personal experiences with children struggling to read. They influence policy and curriculum decisions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, parents_advocating_for_phonics, agenda_setter,
    organized, generational, mobile, national).

% Implement and enforce policies that mandate or strongly recommend systematic phonics instruction, often responding to research and public pressure. They shape curriculum standards and teacher training requirements.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, education_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates reading instruction around a common, evidence-based understanding of how children learn to decode, ensuring consistency across classrooms and schools.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design from individual teachers and diverse approaches to a standardized, phonics-first methodology, supported by specific materials and assessments.
% ABSENT_VOICES: Advocates for holistic, meaning-centered approaches to reading, who argue that an overemphasis on phonics can diminish reading comprehension and enjoyment, are often marginalized in policy discussions driven by 'science of reading' mandates.
% DISAPPEARANCE_RATIONALE: If the primacy of phonics in reading acquisition vanished overnight, educational policy and curriculum would rapidly shift towards more integrated or meaning-based approaches. Teacher training would revert, and the market for decodable texts would shrink, leading to a significant reorganization of literacy education.
% FOUNDING_PROBLEM: Many children, particularly those with learning differences or from disadvantaged backgrounds, struggled to acquire basic decoding skills, leading to widespread illiteracy and academic failure.
% FOUNDING_PROBLEM_CORROBORATION: Literacy researchers (especially those aligned with the 'science of reading'), parents of struggling readers, and educational psychologists consistently attest that the problem of foundational reading skill acquisition remains live, and that explicit phonics is a critical component of the solution. This is corroborated by longitudinal studies on reading outcomes.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).
:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the cost imposed on teachers and schools to adopt specific curricula and training, and the opportunity cost of alternative pedagogical approaches. Suppression (0.60) is moderate to high due to policy mandates and the strong advocacy for this approach, which can marginalize dissenting views and limit pedagogical freedom. Theater ratio is low (0.10) because the instruction is genuinely focused on its stated goal of decoding. Accessibility collapse (0.70) is high as this approach often becomes the default, making alternatives less visible or accessible. Resistance (0.40) exists from educators and researchers who advocate for broader literacy approaches.
 *
 * PERSPECTIVAL GAP:
 *   Teachers trained in whole language or balanced literacy experience this constraint as extractive, forcing them to abandon established practices. Students with dyslexia and from low-literacy homes, however, often experience it as beneficial, providing necessary foundational skills. Policy makers and phonics advocates see it as a necessary coordination mechanism based on scientific evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   Phonics curriculum publishers, 'Science of Reading' researchers, and early career teachers are beneficiaries, gaining market share, validation, and clear instructional paths. Students with dyslexia and from low-literacy homes are also beneficiaries, as the approach directly addresses their learning needs. Teachers trained in whole language are payers, bearing the cost of retraining and pedagogical shift. Parents advocating for phonics and education policy makers act as agenda-setters, driving the adoption and enforcement of this approach.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (to ensure foundational decoding skills) is actively pursued and widely considered live by its proponents. The classification as 'rope' (claimed) reflects the perceived coordination function, while the metrics indicate a degree of extraction and suppression inherent in its implementation, particularly for those whose prior training or pedagogical philosophy differs. The contest is over whether the coordination function is genuinely primary or if the extraction from alternative approaches is disproportionate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_freedom_vs_standardization,
    'Does the standardization imposed by systematic phonics unduly restrict teacher autonomy and pedagogical innovation, or is it a necessary guardrail for effective instruction?',
    'Longitudinal studies comparing student outcomes and teacher satisfaction in highly standardized phonics environments versus those with greater pedagogical freedom, controlling for teacher training and resources.',
    'If standardization is found to stifle effective teaching without commensurate gains, the suppression metric might be re-evaluated upwards, and the constraint could lean more towards a ''tangled_rope'' for teachers. If it proves essential, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_freedom_vs_standardization, conceptual, 'The trade-off between instructional standardization and teacher autonomy.').

omega_variable(
    scope_of_reading_definition,
    'Is ''reading'' primarily decoding, or does it encompass broader meaning-making, critical thinking, and engagement with text from the outset?',
    'Consensus shifts in cognitive science and educational psychology regarding the definition and developmental stages of reading, or a re-evaluation of the empirical evidence for the primacy of decoding over other aspects of literacy.',
    'If the definition of reading broadens, the ''extractiveness'' of a purely phonics-based approach might be seen as higher due to the exclusion of other vital components, potentially shifting the classification towards ''tangled_rope'' or ''snare'' for students whose needs are not met.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_reading_definition, conceptual, 'The fundamental definition of ''reading'' itself, which underpins pedagogical choices.').

omega_variable(
    efficacy_for_all_learners,
    'Is systematic phonics equally effective for all learners, or does its universal application create unintended victims among students who thrive with different instructional approaches?',
    'Comparative studies across diverse student populations, including those with different learning styles, cultural backgrounds, and prior literacy experiences, to identify differential impacts of phonics-first instruction.',
    'If evidence shows significant negative impacts for certain learner groups, the ''victim'' set would expand, and the ''extractiveness'' and ''suppression'' metrics would increase, potentially reclassifying the constraint as a ''tangled_rope'' or ''snare'' due to its asymmetric effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_for_all_learners, empirical, 'The universal applicability and efficacy of systematic phonics across diverse student populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2020, 0.43).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
