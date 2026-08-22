% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Explicit Systematic Phonics as Foundational Reading Instruction
 *   domain: educational/psychological/pedagogical
 *
 * SUMMARY:
 *   This constraint story instantiates the phonics reading of the
 *   reading_acquisition_mechanism kernel: the claim that reading acquisition
 *   requires explicit, systematic instruction in grapheme-phoneme
 *   correspondence as a foundational skill. It is contested by whole-language
 *   and balanced-literacy readings. The constraint operates as a curriculum
 *   mandate enforced by state and district authorities, coordinating literacy
 *   instruction while extracting teacher autonomy and compliance capacity.
 *   The claim/metric independence is maintained: the claimed type is
 *   tangled_rope (genuine coordination plus asymmetric extraction), while
 *   metrics describe moderate extraction, substantial suppression of
 *   alternatives, and rising theater as compliance pressures outpace genuine
 *   implementation fidelity.
 *
 * KEY AGENTS:
 *   - curriculum_authority (agenda_setter, institutional/analytical): sets and enforces phonics mandates, controls standards and evaluation
 *   - classroom_teachers (payer, moderate/constrained): bear autonomy loss and implementation burden, limited exit
 *   - struggling_readers (beneficiary, powerless/trapped): disproportionately gain from guaranteed systematic decoding instruction
 *   - dyslexic_learners (beneficiary, powerless/trapped): depend on explicit phonics for access to literacy
 *   - balanced_literacy_practitioners (excluded, organized/constrained): structurally marginalized from policy and training
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.48).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.65).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Explicit Systematic Phonics as Foundational Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational/psychological/pedagogical").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, 'ef2a6e31-3459-4261-b7cc-15869631900d').
narrative_ontology:cs_kernel_codification('ef2a6e31-3459-4261-b7cc-15869631900d', formalized).
narrative_ontology:cs_authority_grounding('ef2a6e31-3459-4261-b7cc-15869631900d', expertise).
narrative_ontology:cs_interpretation_layer_present('ef2a6e31-3459-4261-b7cc-15869631900d').
narrative_ontology:cs_reading_relation('ef2a6e31-3459-4261-b7cc-15869631900d', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('ef2a6e31-3459-4261-b7cc-15869631900d', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_axiom('ef2a6e31-3459-4261-b7cc-15869631900d', foundational, explicit_gpc_instruction_foundational).
narrative_ontology:cs_axiom_status(explicit_gpc_instruction_foundational, holdable).
narrative_ontology:cs_axiom_grounding('ef2a6e31-3459-4261-b7cc-15869631900d', explicit_gpc_instruction_foundational, empirically_contingent).
narrative_ontology:cs_axiom('ef2a6e31-3459-4261-b7cc-15869631900d', foundational, systematic_scope_sequence_necessary).
narrative_ontology:cs_axiom_status(systematic_scope_sequence_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ef2a6e31-3459-4261-b7cc-15869631900d', systematic_scope_sequence_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('ef2a6e31-3459-4261-b7cc-15869631900d', systematic_explicit_phonics_framework).
narrative_ontology:cs_drift_state('ef2a6e31-3459-4261-b7cc-15869631900d', science_of_reading_resurgence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ef2a6e31-3459-4261-b7cc-15869631900d', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, dyslexic_learners).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets state or district literacy standards mandating scope-and-sequence phonics programs, approves curricula, and monitors compliance through teacher evaluation and assessment regimes. Can shift frameworks based on political and research pressures, but is currently committed to the explicit systematic phonics model.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, curriculum_authority, agenda_setter,
    institutional, generational, analytical, national).

% Must follow mandated scope-and-sequence phonics curricula, reducing autonomy over literacy instruction. Bear training and implementation costs, and face professional consequences for deviating toward integrated or implicit approaches. Exit is constrained by employment requirements and credentialing.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, classroom_teachers, payer,
    moderate, biographical, constrained, regional).

% Receive guaranteed explicit instruction in grapheme-phoneme correspondence that they are unlikely to infer from exposure alone. Depend on the mandate because teacher variability would otherwise leave them without systematic decoding support. Cannot exit the public education system.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Rely disproportionately on structured, explicit phonics instruction to acquire decoding skills. The mandate ensures access to instructional methods aligned with their cognitive profile. Have no exit from compulsory education or from the assigned instructional model.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, dyslexic_learners, beneficiary,
    powerless, biographical, trapped, national).

% Advocate for integrated literacy approaches combining explicit phonics with authentic literature and teacher responsiveness. Are structurally excluded from curriculum design, teacher preparation programs, and policy advisory roles under the phonics mandate. Their professional expertise is officially marginalized.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, balanced_literacy_practitioners, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that all beginning readers, especially those at risk for reading failure, receive explicit, systematic instruction in grapheme-phoneme correspondence rather than depending on teacher variable knowledge or implicit discovery.
% TRANSFER_FUNCTION: Moves curricular autonomy and instructional decision-making from classroom teachers to centralized scope-and-sequence frameworks and curriculum authorities; moves the risk of decoding failure away from individual student discovery toward standardized pedagogical coverage.
% ABSENT_VOICES: Balanced-literacy and whole-language practitioners, who hold that reading develops through integrated meaning-making and that teacher responsiveness matters as much as systematic decoding, are structurally excluded from curriculum committees, teacher-training programs, and policy advisory bodies.
% DISAPPEARANCE_RATIONALE: If the mandate vanished, classroom teachers would regain discretion to integrate literature and implicit strategies; the scope-and-sequence phonics publishing market would contract; struggling readers and dyslexic learners would lose guaranteed systematic decoding exposure and depend on teacher variable preparation; reading outcomes would likely become more heterogeneous across classrooms and districts.
% FOUNDING_PROBLEM: A substantial subset of students failed to acquire accurate decoding skills under implicit, immersion-based, or variable literacy instruction, producing persistent reading failure, costly remediation, and social stratification by literacy ability.
% FOUNDING_PROBLEM_CORROBORATION: Independent meta-analyses from the National Reading Panel and subsequent cognitive psychology research outside the phonics advocacy community corroborate that explicit phonics improves decoding for at-risk populations; literacy educators from constructivist traditions contest that the problem is instructional method rather than socioeconomic inequality or resource distribution, attesting from outside the beneficiary set.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.48, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.48) is moderate-to-substantial: the mandate imposes real costs in teacher discretion and implementation overhead that are not fully internal to the coordination function. Suppression (0.65) is higher because the constraint's persistence requires actively marginalizing balanced-literacy and whole-language alternatives in teacher preparation and materials markets. Theater ratio (0.35 and rising) reflects growing implementation fidelity gapsâteachers performing phonics routines while retaining covert balanced-literacy practices. Accessibility collapse (0.72) is high: once the phonics mandate is installed, alternative pedagogical frameworks lose institutional standing and become practically inaccessible within the regulated system. Resistance (0.50) reflects sustained pushback from teaching professionals and literacy researchers outside the phonics consensus.
 *
 * PERSPECTIVAL GAP:
 *   The curriculum authority seat experiences the constraint as necessary coordination: without the mandate, teacher variability leaves at-risk students without systematic decoding. The classroom teacher seat experiences the same structure as extraction of professional judgment and compliance burden. The engine computes this divergence from structural dataâsame constraint, opposed directionalitiesâwithout requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers and dyslexic learners are declared beneficiaries with trapped exit, placing them at the low-d (subsidy) end of the spectrum. Classroom teachers are declared victims/payers with constrained exit, placing them at the high-d (target) end. Curriculum authorities are agenda setters without beneficiary or victim declarations; they default to their power atom's canonical directionality, likely near symmetric. The high extraction experienced by teachers is amplified by their constrained exit and moderate scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâreading failure under implicit instructionâis still live, corroborated by independent reading research. The constraint has not outlived its function: populations of struggling readers still exist and still benefit from explicit decoding instruction. Therefore mandatrophy is not declared. If the founding problem were solved (universal literacy regardless of instructional model) and the constraint persisted on inertia, it would degrade toward piton. The live founding problem protects against false mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is explicit systematic phonics instruction a necessary natural feature of reading acquisition or a constructed pedagogical commitment that privileges one literacy tradition over others?',
    'Comparative efficacy trials across diverse student populations with long-term follow-up controlling for socioeconomic confounds, or historical analysis of literacy acquisition prior to systematic phonics schooling.',
    'If reading acquisition is possible through multiple routes with equivalent population outcomes, this constraint is a constructed coordination mechanism rather than a pedagogical mountain; if explicit phonics is strictly necessary for most learners, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether phonics mandate reflects natural learning necessity or constructed policy preference').

omega_variable(
    teacher_discretion_vs_mandate,
    'Does the narrowing of teacher discretion represent unavoidable coordination overhead or unnecessary extraction of professional judgment?',
    'Outcome comparison between classrooms with high teacher autonomy but phonics-trained teachers versus scope-and-sequence-mandated classrooms; if outcomes are equivalent for at-risk students, the mandate extracts without added value.',
    'If autonomy produces equivalent outcomes, extraction rises; if autonomy produces inferior outcomes for at-risk students, the coordination function justifies the discretion loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_discretion_vs_mandate, empirical, 'Whether teacher discretion loss is necessary coordination cost or extractive overhead').

omega_variable(
    sibling_reading_structural_pressure,
    'How would the classification change if evaluated under the balanced-literacy or whole-language sibling reading of the same kernel?',
    'Cross-reading corpus analysis: the balanced-literacy reading likely classifies as rope or tangled-rope with lower suppression; the whole-language reading might classify as scaffold or piton depending on enforcement history. Independent corpus entries for sibling readings are required for full kernel topology.',
    'This reading''s extraction and enforcement levels may be specific to the phonics commitment; sibling readings may show lower suppression and higher teacher autonomy, shifting type toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_pressure, conceptual, 'Structural relationship to sibling kernel readings and their expected types').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phonics_reading_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(phonics_reading_tr_t5, reading_acquisition_mechanism__phonics_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(phonics_reading_tr_t10, reading_acquisition_mechanism__phonics_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(phonics_reading_tr_t15, reading_acquisition_mechanism__phonics_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(phonics_reading_tr_t20, reading_acquisition_mechanism__phonics_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(phonics_reading_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(phonics_reading_be_t5, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(phonics_reading_be_t10, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(phonics_reading_be_t15, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(phonics_reading_be_t20, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(phonics_reading_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(phonics_reading_su_t5, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(phonics_reading_su_t10, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(phonics_reading_su_t15, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(phonics_reading_su_t20, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, resource_allocation).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, whole_language_reading).

% DUAL FORMULATION NOTE:
% The reading_acquisition_mechanism kernel decomposes into three structurally distinct readings: phonics_reading (systematic explicit phonics as foundational), balanced_literacy_reading (integrated phonics and authentic literature), and whole_language_reading (implicit decoding through text exposure). Each instantiates a different constraint with distinct beneficiary/victim structures, epsilon values, and enforcement requirements. They compete for the same institutional spaceâcurriculum standards, teacher training, publishing marketsâbut rest on incompatible empirical and normative premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
