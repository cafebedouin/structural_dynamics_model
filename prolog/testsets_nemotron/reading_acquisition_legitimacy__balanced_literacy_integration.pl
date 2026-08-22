% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Balanced literacy presents itself as a research-informed synthesis of
 *   phonics and whole language, but operates as a tangled rope: it
 *   coordinates instructional coherence across classrooms (genuine
 *   coordination function) while extracting measurable harms from vulnerable
 *   readers through its commitment to three-cueing, leveled texts, and
 *   embedded rather than systematic phonics. The constraint is actively
 *   enforced through curriculum adoption cycles, teacher evaluation rubrics,
 *   and professional gatekeeping. Struggling readers — especially those with
 *   dyslexia and from economically disadvantaged backgrounds — pay the
 *   extraction cost in the form of preventable reading failure. The
 *   beneficiary coalition (publishers, consultants, teacher educators, whole
 *   language advocates) maintains the arrangement through professional
 *   identity, institutional inertia, and control of professional development
 *   pipelines.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.45).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.35).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, '3229f52f-ca0b-4a71-a0ed-b20b97090c5e').
narrative_ontology:cs_kernel_codification('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', distributed).
narrative_ontology:cs_authority_grounding('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', practice).
narrative_ontology:cs_interpretation_layer_present('3229f52f-ca0b-4a71-a0ed-b20b97090c5e').
narrative_ontology:cs_reading_relation('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', reading_acquisition_legitimacy__whole_language_meaning_primacy, influences).
narrative_ontology:cs_reading_relation('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', foundational, synthesis_of_decoding_and_meaning_is_legitimate_ideal).
narrative_ontology:cs_axiom_status(synthesis_of_decoding_and_meaning_is_legitimate_ideal, holdable).
narrative_ontology:cs_axiom_grounding('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', synthesis_of_decoding_and_meaning_is_legitimate_ideal, instrumental).
narrative_ontology:cs_axiom('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', foundational, teacher_responsive_judgment_mediates_instruction).
narrative_ontology:cs_axiom_status(teacher_responsive_judgment_mediates_instruction, holdable).
narrative_ontology:cs_axiom_grounding('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', teacher_responsive_judgment_mediates_instruction, conventional).
narrative_ontology:cs_axiom('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', secondary, authentic_literature_exposure_is_necessary_condition).
narrative_ontology:cs_axiom_status(authentic_literature_exposure_is_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', authentic_literature_exposure_is_necessary_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', balanced_literacy_synthesis_1990s).
narrative_ontology:cs_drift_state('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', post_science_of_reading_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3229f52f-ca0b-4a71-a0ed-b20b97090c5e', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_consultants_balanced).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_education_programs_balanced).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, economically_disadvantaged_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_without_adequate_training).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, dual_route_reading_model).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_professional_judgment_primacy).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, developmentally_appropriate_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and sell leveled readers, guided reading materials, and balanced literacy curricula adopted by districts nationwide. Revenue depends on districts maintaining balanced literacy adoption cycles. Can pivot to structured literacy materials if market shifts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Provide professional development, coaching, and curriculum implementation support for balanced literacy frameworks. Income tied to district contracts for balanced literacy training. Retraining for structured literacy requires substantial investment.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_consultants_balanced, beneficiary,
    organized, biographical, constrained, national).

% Prepare pre-service teachers in balanced literacy methodologies. Accreditation and enrollment depend on alignment with dominant pedagogical paradigms. Curriculum revision is slow and politically contested within faculties of education.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_education_programs_balanced, beneficiary,
    institutional, generational, constrained, national).

% Professional identity and scholarly reputation built on whole language/balanced literacy paradigm. See balanced literacy as the legitimate synthesis protecting meaning-centered instruction. Resistance to structured literacy framed as defending teacher autonomy and child-centered practice.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_advocates, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_advocates, agenda_setter).

% Children who fail to achieve reading proficiency under balanced literacy's implicit phonics approach. Experience cumulative academic failure, behavioral consequences, and long-term reduced life outcomes. Cannot exit the school system; dependent on whatever instruction their school provides.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Students with dyslexia who require explicit, systematic, cumulative phonics instruction. Balanced literacy's embedded phonics and three-cueing strategies are contraindicated by research. Often identified late or not at all; receive interventions that don't match their neurocognitive needs.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Students from low-income backgrounds who enter school with less literacy exposure and depend entirely on school-based instruction for reading acquisition. Balanced literacy's reliance on implicit learning and authentic texts disproportionately harms those without home literacy enrichment. Cannot access private tutoring or specialized schools.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, economically_disadvantaged_students, payer,
    powerless, biographical, trapped, local).

% Classroom teachers expected to implement complex balanced literacy frameworks (running records, guided reading, word study, shared reading) with minimal pre-service preparation and inconsistent ongoing support. Bear professional accountability for student outcomes without control over curriculum adoption or adequate professional development.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_without_adequate_training, payer,
    moderate, biographical, constrained, local).

% Researchers, clinicians, parent advocates, and educators arguing for explicit systematic phonics as the evidence-based foundation. Marginalized in curriculum adoption processes, teacher preparation, and professional organizations dominated by balanced literacy paradigm. Gain influence through state legislation and dyslexia advocacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, structured_literacy_advocates, excluded,
    organized, generational, constrained, national).

% Scientists studying reading acquisition, neural mechanisms of literacy, and instructional efficacy. Provide evidence on phonological awareness, orthographic mapping, and the necessity of explicit instruction for most learners. Not direct participants in policy battles but their findings structure the contested terrain.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, cognitive_science_researchers_literacy, observer,
    analytical, civilizational, analytical, universal).

% Enact literacy legislation (dyslexia screening, curriculum mandates, teacher preparation requirements) that can override local balanced literacy adoption. Respond to parent advocacy and media pressure. Some captured by education establishment; others drive reform.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, state_legislators_education_committees, agenda_setter,
    institutional, biographical, mobile, national).

% Select and purchase literacy curricula, design professional development, and evaluate program effectiveness. Caught between state mandates, vendor relationships, teacher preferences, and parent pressure. Career incentives favor stability and consensus over disruptive change.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, district_curriculum_directors, agenda_setter,
    institutional, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common pedagogical framework that allows teachers to integrate decoding instruction with authentic reading experiences, creating a shared professional language across grade levels and enabling district-wide curriculum coherence.
% TRANSFER_FUNCTION: Moves instructional time, professional development resources, and curriculum purchasing authority toward balanced literacy materials and approaches; moves the cost of reading failure onto struggling readers (especially dyslexic and economically disadvantaged students) who receive insufficiently explicit instruction.
% ABSENT_VOICES: Students themselves (especially struggling readers and dyslexic students) are structurally excluded from curriculum decisions. Parents of dyslexic children were historically excluded until organized advocacy emerged. Cognitive scientists studying reading acquisition were long marginalized in education policy circles.
% DISAPPEARANCE_RATIONALE: If balanced literacy mandates disappeared overnight, districts would rapidly adopt structured literacy curricula (driven by state legislation and vendor response), teacher preparation programs would restructure coursework, professional development would pivot to explicit phonics, and struggling readers would receive more effective intervention sooner — the entire early literacy ecosystem would reorganize.
% FOUNDING_PROBLEM: Mid-20th century reading instruction polarized between rigid phonics-first basal readers and unstructured whole language. Balanced literacy emerged in the 1990s as a synthesis promising the best of both: explicit phonics embedded in authentic literature experiences, guided by responsive teacher judgment.
% FOUNDING_PROBLEM_CORROBORATION: Balanced literacy proponents (Fountas, Pinnell, Calkins) attest the synthesis remains the correct framework. Cognitive scientists (Seidenberg, Stanovich, Ehri) and structured literacy advocates (IDA, Reading League) attest the founding synthesis was empirically flawed — phonics cannot be effectively 'balanced' when embedded in three-cueing systems. State legislatures increasingly side with the latter through dyslexia mandates.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).
:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the measurable opportunity cost: students who would succeed with explicit systematic phonics but fail under balanced literacy's implicit approach. Suppression (0.35) captures the active marginalization of structured literacy alternatives — not through formal bans but through curriculum gatekeeping, professional ostracism, and control of teacher preparation. Theater ratio (0.28) reflects genuine coordination value (shared framework, authentic literature) mixed with performative adherence to 'balance' while maintaining practices contradicted by evidence. Accessibility collapse (0.42) and resistance (0.55) indicate alternatives exist and are actively contested — not a mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary/agenda-setter seats, balanced literacy is a flexible professional framework supporting teacher judgment. From the victim seats, it is a system that withholds the explicit instruction they need. The engine computes this divergence from the structural data — the same constraint registers as coordination for some, extraction for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (publishers, consultants, teacher educators, whole language advocates) collect revenue, professional status, and institutional control — directionality near 0.0. Victims (struggling readers, dyslexic students, economically disadvantaged students, undertrained teachers) bear costs with minimal exit — directionality near 1.0. Agenda-setters (legislators, district directors) sit near 0.5: they can change the system but face opposing pressures. Excluded structured literacy advocates are structurally blocked but gaining leverage. Observer cognitive scientists see the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (polarized phonics vs. whole language) was real in 1990. Balanced literacy's synthesis was a genuine coordination attempt. But the mandate persists despite evidence that its specific implementation (three-cueing, leveled texts, embedded phonics) harms vulnerable learners. The coordination function has atrophied into a protection racket for the beneficiary coalition. Mandatrophy is unresolved — the arrangement survives by suppressing the evidence that would obsolete it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthesis_vs_protection_racket,
    'Does balanced literacy genuinely synthesize phonics and meaning-centered instruction, or does it structurally protect whole language commitments by absorbing phonics rhetoric without changing core practices?',
    'Classroom observation studies comparing enacted balanced literacy instruction vs. structured literacy: measure explicitness, systematicity, cumulativity, and diagnostic responsiveness of phonics instruction. Longitudinal outcomes for at-risk readers under each.',
    'If protection racket, the constraint is extractive coordination (tangled_rope/snare) not genuine synthesis. If genuine synthesis, extraction metrics should be lower and coordination higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthesis_vs_protection_racket, empirical, 'Whether the ''balanced'' label describes enacted practice or rhetorical cover.').

omega_variable(
    teacher_judgment_vs_evidence_base,
    'Is ''teacher professional judgment'' in balanced literacy a genuine coordination mechanism (responsive adaptation to students) or a structural shield against evidence-based mandates?',
    'Analyze professional development content, teacher evaluation rubrics, and curriculum materials: does ''professional judgment'' correlate with adopting evidence-based practices when evidence shifts, or with maintaining incumbent practices?',
    'If shield, the coordination function is theatrical and the constraint trends toward snare/ piton. If genuine, the constraint retains adaptive coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_judgment_vs_evidence_base, conceptual, 'Whether professional autonomy rhetoric enables or blocks evidence integration.').

omega_variable(
    three_cueing_causal_role,
    'Is the three-cueing system (MSV) taught in balanced literacy a necessary scaffold for meaning-making, or a causal mechanism of reading failure for vulnerable learners?',
    'Experimental studies comparing decoding outcomes with and without three-cueing instruction, controlling for phonics dosage. Neuroimaging of word recognition strategies.',
    'If causal mechanism of failure, the constraint''s extraction is direct and measurable. If neutral or beneficial, extraction metrics overstate harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(three_cueing_causal_role, empirical, 'Causal status of the core balanced literacy decoding strategy.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''reading_acquisition_legitimacy'' kernel admit a single legitimate reading, or are the sibling readings (phonics_decoding_primacy, whole_language_meaning_primacy, structured_literacy_remediation) structurally incommensurable framings that cannot be synthesized?',
    'Analyze whether any single instructional framework can satisfy the core premises of all four readings simultaneously without logical contradiction.',
    'If incommensurable, the kernel is a false unity masking irreconcilable commitments. If synthesizable, a genuine integrated reading may exist beyond current contested versions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s sibling readings are logically compatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t1997, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1997, 0.15).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t2005, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t2015, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t2020, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t2025, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t1997, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1997, 0.3).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t2005, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t2015, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t2020, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t2025, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t1997, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1997, 0.25).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t2005, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2010, 0.32).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t2015, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2015, 0.34).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t2020, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t2025, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__balanced_literacy_integration, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__structured_literacy_remediation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_preparation_literacy_mandates).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, state_dyslexia_screening_legislation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_adoption_cycles_k12).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'reading wars' into four structurally distinct legitimacy claims sharing the kernel 'reading_acquisition_legitimacy'. Balanced literacy integration claims synthesis but functions as whole language protection. Phonics decoding primacy claims alphabetic principle as non-negotiable foundation. Structured literacy remediation claims the vulnerable learner as the design criterion. Whole language meaning primacy claims meaning-making as the essence of reading. Each has different ε, different beneficiary/victim structures, different enforcement mechanisms. They are linked here because each cites the others as foil, and policy battles shift resources between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, institutional, 0.15).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, organized, 0.25).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, powerless, 0.95).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
