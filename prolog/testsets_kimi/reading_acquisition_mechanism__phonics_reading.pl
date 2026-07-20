% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Systematic Phonics as Foundational Reading Acquisition Mandate
 *   domain: educational/psychological/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the phonics reading of the contested
 *   reading_acquisition_mechanism kernel: the claim that reading acquisition
 *   requires explicit systematic instruction in grapheme-phoneme
 *   correspondence as a foundational skill. When this claim is
 *   institutionalized as curriculum mandate, teacher evaluation criterion,
 *   and funding condition, it functions as a constraint that coordinates
 *   instructional practice while asymmetrically extracting pedagogical
 *   autonomy from teachers and marginalizing whole-language practitioners.
 *   The constraint is claimed as rope (scientifically necessary coordination)
 *   while the authored metrics track tangled rope characteristics: genuine
 *   coordination value for beginning readers combined with active
 *   enforcement, suppression of alternatives, and capture by commercial
 *   curriculum interests.
 *
 * KEY AGENTS:
 *   - classroom_teachers: Primary target (moderate/constrained) â bear extraction through lost autonomy and scripted instruction
 *   - struggling_readers: Primary beneficiary (powerless/trapped) â receive structured decoding instruction hypothesized to benefit them
 *   - phonics_curriculum_publishers: Secondary beneficiary (powerful/arbitrage) â collect revenue from mandated programs
 *   - district_administrators: Agenda setter (institutional/constrained) â enforce compliance through evaluation and procurement
 *   - whole_language_educators: Excluded/victim (moderate/constrained) â pedagogical tradition suppressed, expertise devalued
 *   - reading_science_researchers: Analytical observer (analytical/analytical) â provide empirical legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.55).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.6).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Systematic Phonics as Foundational Reading Acquisition Mandate").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational/psychological/institutional").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '834ac335-74e4-4734-ab03-8e52d6fa6b9d').
narrative_ontology:cs_kernel_codification('834ac335-74e4-4734-ab03-8e52d6fa6b9d', distributed).
narrative_ontology:cs_authority_grounding('834ac335-74e4-4734-ab03-8e52d6fa6b9d', expertise).
narrative_ontology:cs_interpretation_layer_present('834ac335-74e4-4734-ab03-8e52d6fa6b9d').
narrative_ontology:cs_reading_relation('834ac335-74e4-4734-ab03-8e52d6fa6b9d', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('834ac335-74e4-4734-ab03-8e52d6fa6b9d', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('834ac335-74e4-4734-ab03-8e52d6fa6b9d', foundational, explicit_systematic_phonics_necessary).
narrative_ontology:cs_axiom_status(explicit_systematic_phonics_necessary, holdable).
narrative_ontology:cs_axiom_grounding('834ac335-74e4-4734-ab03-8e52d6fa6b9d', explicit_systematic_phonics_necessary, empirically_contingent).
narrative_ontology:cs_axiom('834ac335-74e4-4734-ab03-8e52d6fa6b9d', foundational, scope_sequence_mandatory).
narrative_ontology:cs_axiom_status(scope_sequence_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('834ac335-74e4-4734-ab03-8e52d6fa6b9d', scope_sequence_mandatory, empirically_contingent).
narrative_ontology:cs_reference_frame('834ac335-74e4-4734-ab03-8e52d6fa6b9d', systematic_phonics_foundation).
narrative_ontology:cs_drift_state('834ac335-74e4-4734-ab03-8e52d6fa6b9d', contemporary_policy_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('834ac335-74e4-4734-ab03-8e52d6fa6b9d', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, phonics_curriculum_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, whole_language_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must follow mandated systematic phonics scope-and-sequence, often using scripted curricula. Loses discretion to adapt instruction to individual student needs or integrate literature-based approaches. Exit means leaving the profession or teaching in non-compliant private or charter contexts.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, classroom_teachers, payer,
    moderate, biographical, constrained, national).

% Receive explicit decoding instruction hypothesized to benefit them disproportionately. Cannot opt out of the systematic approach even if they might thrive under alternative methods. The arrangement provides structured decoding instruction but does not allow flexibility to try alternative approaches.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Sell systematic phonics programs, decodable readers, and assessment tools mandated by the arrangement. Revenue scales with mandate enforcement breadth. Can pivot product lines if policy shifts.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, phonics_curriculum_publishers, beneficiary,
    powerful, generational, arbitrage, national).

% Implement state or federal mandates for evidence-based reading instruction. Enforce compliance through curriculum adoption, teacher evaluation, and progress monitoring. Bear political accountability for literacy outcomes but lack authority to override the scientific framing of the mandate.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, district_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Their pedagogical expertise and materials are marginalized by the mandate. Excluded from curriculum decisions and teacher preparation programs that have shifted to structured literacy. Would argue for text-rich, implicit approaches but are not in the policy conversation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_educators, excluded,
    moderate, biographical, constrained, national).

% Conduct empirical studies comparing instructional approaches. Provide the evidence base cited by the mandate. Some specialize in cognitive science of reading; others study implementation. Not directly governed by the arrangement but shape its legitimacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, reading_science_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes early reading instruction across classrooms and schools to ensure all students receive explicit grapheme-phoneme correspondence instruction, reducing instructional variance and creating predictable developmental progressions from decoding to fluency.
% TRANSFER_FUNCTION: Moves pedagogical autonomy from classroom teachers to centralized curriculum designers and scope-and-sequence authors; moves institutional funding toward systematic phonics program vendors and away from literature-based materials; moves legitimacy from whole-language research traditions to experimental cognitive science.
% ABSENT_VOICES: Whole-language educators and teachers trained in balanced literacy approaches are structurally excluded from curriculum committees and textbook adoption processes; their research tradition is treated as discredited rather than contested. Students who might acquire reading more readily through implicit statistical learning are not represented in the mandate design.
% DISAPPEARANCE_RATIONALE: If the mandate vanished, classroom teachers would revert to heterogeneous instructional practices including eclectic and literature-based methods, curriculum procurement would shift away from systematic phonics programs, teacher preparation programs would rebalance toward comprehension and authentic text, and the current standardization of early reading outcomes would dissolve into local variance.
% FOUNDING_PROBLEM: High rates of reading failure and inequitable outcomes, particularly for students not acquiring decoding skills incidentally from text exposure; lack of consistent instructional methodology across classrooms leading to some students receiving no explicit phonics instruction.
% FOUNDING_PROBLEM_CORROBORATION: Reading science researchers and policy advocates attest the problem is live, citing persistent NAEP scores. Whole-language educators and some literacy scholars contest that the founding problem was primarily lack of phonics rather than poverty, resource inequity, or inadequate teacher preparation; they note that reading failure persists in heavily phonics-aligned jurisdictions, suggesting the problem is misdiagnosed. Corroboration from economists studying instructional input-output is mixed.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.55) reflects the asymmetric cost-benefit structure: struggling readers gain decoding coordination, but teachers lose substantial professional discretion and alternative pedagogies are excluded. Suppression (0.60) captures the active institutional suppression of whole-language and balanced-literacy alternatives through mandate structures, material de-funding, and teacher-prep program restructuring. Theater_ratio (0.40) acknowledges that a growing share of 'science of reading' implementation involves performative compliance (purchasing phonics programs, using terminology) without fidelity to systematic scope-and-sequence practice. Accessibility_collapse (0.70) is high because whole-language teacher preparation pathways and materials have largely disappeared from institutional availability. Resistance (0.45) reflects significant but politically losing pushback from educator groups and whole-language scholars. The temporal measurements show two waves: the Reading First era (2000-2008) and the contemporary Science of Reading mandate wave (2016-2024), with a dip between them when enforcement relaxed and practice drifted toward balanced literacy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (district administrators) and beneficiary seats (publishers, struggling readers) experience the constraint as necessary coordination that solves instructional variance. The payer seat (classroom teachers) experiences it as top-down extraction of professional judgment. The excluded seat (whole-language educators) experiences it as erasure. The engine computes this divergence from structural data: teachers have constrained exit and bear direct autonomy costs, while publishers and district administrators face less constrained positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers are structural beneficiaries (d near the beneficiary end) because the arrangement subsidizes their instruction with systematic support; their trapped exit amplifies coordination but does not increase extraction. Classroom teachers are structural targets (d near the target end) because the arrangement removes their discretion and substitutes external scope-and-sequence authority; constrained exit amplifies effective extraction. Curriculum publishers sit near beneficiary despite powerful status because they profit from the constraint's operation. Whole-language educators sit near full target because the arrangement extracts their professional standing and excludes their practice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling by distinguishing the genuine coordination function (standardized decoding instruction for beginners) from the extractive overlay (teacher deskilling, commercial program mandate, methodological monoculture). Without the beneficiary declaration for struggling readers, the constraint would compute as snare; without the victim declaration for teachers and whole-language educators, it would compute as rope. The temporal series reveal metric substitution (theater_ratio rising as enforcement outpaces fidelity) and extraction accumulation (base_extractiveness rising across mandate waves), which are hallmarks of tangled rope drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_gap,
    'Does the mandated systematic phonics instruction produce the claimed outcomes in actual classroom implementation, or does implementation drift create a theater-heavy extraction layer?',
    'Large-scale classroom observation studies measuring fidelity to scope-and-sequence versus student outcome correlation.',
    'If fidelity is low and outcomes weak, the coordination function is theater and extraction dominates; if fidelity is high and outcomes strong, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_gap, empirical, 'Whether classroom reality matches the systematic phonics ideal.').

omega_variable(
    teacher_autonomy_cost,
    'Is the loss of teacher pedagogical discretion a necessary cost of instructional standardization, or an extractive surplus that benefits curriculum publishers and administrators?',
    'Comparative analysis of high-autonomy versus low-autonomy phonics implementations with equivalent student outcomes.',
    'If equivalent outcomes exist with higher autonomy, the narrowed discretion is surplus extraction; if only scripted programs succeed, it is necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_cost, conceptual, 'Whether teacher deskilling is necessary coordination cost or extractive surplus.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the phonics reading logically foreclose the whole_language reading, or do they coexist as complementary developmental mechanisms?',
    'Developmental psychology research on whether implicit statistical learning mechanisms can produce decoding without explicit instruction in typical or atypical populations.',
    'If foreclosure is absolute, the kernel is zero-sum; if complementary, balanced_literacy or developmental-stage synthesis is possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether phonics and whole-language readings are logically mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_mechanism__phonics_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__phonics_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_mechanism__phonics_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__phonics_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__phonics_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__phonics_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(read_be_t4, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(read_be_t12, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 24, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(read_su_t4, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(read_su_t12, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The reading_acquisition_mechanism kernel decomposes into three structurally distinct constraints because the colloquial label conflates competing empirical claims about reading acquisition. Phonics_reading (this file) asserts high extraction when enforced as mandate due to autonomy costs; whole_language_reading would instantiate a different constraint with different victim/beneficiary structures; balanced_literacy_reading would instantiate a hybrid. Each reading has distinct epsilon, stakeholders, and enforcement requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
