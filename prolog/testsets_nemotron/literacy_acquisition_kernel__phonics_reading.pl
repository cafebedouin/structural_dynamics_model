% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Explicit Systematic Phonics-First Reading Instruction
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint story instantiates the phonics_reading of the contested
 *   literacy_acquisition_kernel. The reading asserts that reading acquisition
 *   requires explicit, systematic instruction in phoneme-grapheme
 *   correspondence before connected text exposure, with decoding as the
 *   prerequisite skill that enables comprehension. Historically, this reading
 *   gained dominance after the National Reading Panel (2000) and subsequent
 *   IES practice guides synthesized evidence for systematic phonics. The
 *   constraint operates through state-level mandates, curriculum adoption
 *   policies, and teacher preparation requirements that require scripted,
 *   sequential phonics programs. The structural delta is asymmetric: students
 *   with weak phonological awareness (including dyslexia-risk students) are
 *   clear beneficiaries — systematic instruction reduces their decoding
 *   failure rate substantially. Teachers' professional judgment is the
 *   identified victim — scripted programs remove discretion over pacing,
 *   sequence, and responsive adaptation, extracting autonomy without
 *   commensurate gain in outcomes for all students. The constraint is a
 *   tangled_rope because it simultaneously solves a genuine coordination
 *   problem (ensuring all students receive evidence-based decoding
 *   instruction) and extracts from a professional group (teachers) whose
 *   situated judgment is displaced by protocol.
 *
 * KEY AGENTS:
 *   - students_with_weak_phonological_awareness: Primary beneficiary (moderate/constrained) — decoding failure drops dramatically under systematic instruction
 *   - students_with_dyslexia_risk: Primary beneficiary (moderate/constrained) — structured literacy prevents the Matthew effect cascade
 *   - teachers_professional_judgment: Primary victim (organized/constrained) — scripted lessons remove diagnostic responsiveness and professional discretion
 *   - literacy_researchers: Agenda setter (institutional/biographical) — synthesize evidence, define what counts as systematic
 *   - state_education_agencies: Agenda setter (institutional/generational) — mandate adoption, enforce fidelity
 *   - curriculum_publishers: Beneficiary (powerful/mobile) — capture the mandated market for scripted programs
 *   - whole_language_practitioners: Excluded (organized/trapped) — their epistemic framework is ruled out by mandate
 *   - balanced_literacy_practitioners: Excluded (organized/constrained) — their integrative approach is treated as non-compliant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.48).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.35).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Explicit Systematic Phonics-First Reading Instruction").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '77bd5a40-26fb-4f0e-9d40-3a17242ccb4d').
narrative_ontology:cs_kernel_codification('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', formalized).
narrative_ontology:cs_authority_grounding('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', expertise).
narrative_ontology:cs_interpretation_layer_present('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d').
narrative_ontology:cs_reading_relation('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', foundational, decoding_precedes_comprehension).
narrative_ontology:cs_axiom_status(decoding_precedes_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', decoding_precedes_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', foundational, systematic_phoneme_grapheme_instruction_required).
narrative_ontology:cs_axiom_status(systematic_phoneme_grapheme_instruction_required, holdable).
narrative_ontology:cs_axiom_grounding('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', systematic_phoneme_grapheme_instruction_required, empirically_contingent).
narrative_ontology:cs_reference_frame('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', national_reading_panel_consensus).
narrative_ontology:cs_drift_state('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', state_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77bd5a40-26fb-4f0e-9d40-3a17242ccb4d', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_dyslexia_risk).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, decoding_precedes_comprehension).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, systematic_phoneme_grapheme_instruction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These students enter school with limited ability to segment and blend phonemes. Under systematic phonics instruction, their decoding accuracy improves substantially and reliably. Without it, they experience the Matthew effect — falling further behind each year. They cannot exit the school system; their only exit is within-instruction responsiveness, which scripted programs limit. Their benefit is large and structurally necessary.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    moderate, biographical, constrained, national).

% Students with or at risk for dyslexia require explicit, cumulative, multisensory instruction in phoneme-grapheme correspondence. Systematic phonics programs (especially structured literacy variants) prevent the cascade of failure across reading, writing, and content-area learning. They are constrained to whatever instruction their school provides; private tutoring is an exit only for resourced families. Their benefit is the difference between literacy and functional illiteracy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_dyslexia_risk, beneficiary,
    moderate, biographical, constrained, national).

% Teachers lose discretion over instructional sequence, pacing, and responsive adaptation when mandated to implement scripted phonics programs with fidelity monitoring. Their professional knowledge of student needs, developmental readiness, and instructional nuance is displaced by protocol. Exit options: leave the profession (costly), move to non-mandated grades/settings (limited), or comply while privately adapting (risk of sanction). The extraction is the gap between what a responsive teacher would do and what the script requires.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment, payer,
    organized, biographical, constrained, national).

% Researchers synthesize evidence (e.g., National Reading Panel, meta-analyses) and define what counts as 'systematic' and 'explicit.' They set the evidence base that mandates cite. Their exit is analytical — they can revise conclusions as evidence evolves — but institutional citation of their work creates path dependence. They benefit from epistemic authority but do not directly collect rents.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, literacy_researchers, agenda_setter,
    institutional, biographical, analytical, global).

% State agencies mandate adoption of approved phonics programs, require fidelity monitoring, and tie funding/accountability to compliance. They can revise mandates but face political costs (legislative pressure, parent advocacy, media narratives). Their role is enforcement and legitimization; they extract compliance from districts and teachers.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, state_education_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Publishers of scripted phonics programs capture the mandated market. They design programs to meet state rubrics, lobby for adoption, and profit from recurring purchases and professional development contracts. They have high exit — they can pivot to other markets or program types. Their benefit is direct revenue capture from the constraint's enforcement.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers, beneficiary,
    powerful, biographical, mobile, global).

% Teachers and scholars committed to whole language pedagogy (reading emerges from meaningful text engagement; phonics develops naturally) are structurally excluded from approved curriculum lists and teacher preparation programs. Their epistemic framework is ruled out by mandate. They cannot exit the constraint's domain (public schooling) without leaving the profession. They would object that the constraint misrepresents reading development and harms student motivation.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_practitioners, excluded,
    organized, biographical, trapped, national).

% Practitioners who integrate systematic phonics with meaningful text engagement (reading workshop, guided reading) find their approach treated as non-compliant because it does not enforce strict precedence (decoding before connected text) or scripted fidelity. They have more exit than whole_language_practitioners (can adapt within mandates) but face pressure to adopt approved programs. They would argue the constraint creates a false dichotomy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, balanced_literacy_practitioners, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures all students, especially those with weak phonological awareness, receive explicit, systematic instruction in the alphabetic principle — solving the coordination problem of inconsistent, incidental, or absent decoding instruction that leaves vulnerable students behind.
% TRANSFER_FUNCTION: Moves instructional autonomy (pacing, sequence, responsive adaptation) from teachers to protocol designers and state mandates; moves decoding reliability from chance to systematic guarantee for vulnerable students; moves market revenue to curriculum publishers.
% ABSENT_VOICES: Whole language practitioners and balanced literacy practitioners are structurally excluded from policy-making tables where mandates are designed. Students themselves (especially young children) have no voice. Parents of dyslexic children are often the loudest advocates FOR the mandate, creating a split in the 'parent voice.'
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, districts would revert to locally chosen curricula — many would adopt balanced literacy or whole language approaches; decoding failure rates for vulnerable students would likely rise; teacher autonomy would return; publisher revenue from scripted programs would collapse. The educational ecosystem would reorganize.
% FOUNDING_PROBLEM: By the 1990s, whole language dominance had produced a cohort of students — disproportionately low-income, with weak phonological awareness — who could not decode. The National Reading Panel (2000) was convened to settle the 'reading wars' and found strong evidence for systematic phonics. The founding problem was: how to ensure all children, especially the most vulnerable, acquire the alphabetic principle reliably.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (universal decoding acquisition) is attested as still live by dyslexia advocacy organizations (e.g., International Dyslexia Association), cognitive scientists (e.g., Seidenberg, Wolf), and NAEP data showing persistent below-basic reading rates. It is attested as substantially solved for basic decoding by some reading researchers who argue the mandate now overreaches into comprehension instruction. No single external body corroborates both the problem's persistence and the mandate's continued fit — the contested status is the signal.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.48) reflects the asymmetric transfer: teachers lose professional autonomy (scripted pacing, prescribed sequence, fidelity monitoring) while students with weak phonological awareness gain decoding reliability. The extraction is not maximal because the student benefit is real and substantial — this is not pure extraction. Suppression (0.35) is moderate: the constraint persists through policy mandates and accreditation requirements, not brute force, but alternative pedagogies are structurally excluded from adoption lists. Theater ratio (0.18) is low: the instruction is functional, not performative, though fidelity monitoring introduces some compliance theater. Accessibility collapse (0.42) is partial: alternatives exist (balanced literacy, whole language) but are marginalized in policy space. Resistance (0.55) is significant: teacher unions, education faculties, and practitioner networks actively contest mandates. The claimed type tangled_rope captures the dual structure: genuine coordination for vulnerable students + extraction from teacher professional judgment.
 *
 * PERSPECTIVAL GAP:
 *   From the student-with-dyslexia seat, this constraint is a rope (pure coordination — it solves the decoding bottleneck). From the teacher seat, it is a snare (extraction of professional judgment via scripted compliance). From the researcher seat, it is a mountain (the evidence base is treated as settled science). The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: students_with_weak_phonological_awareness and students_with_dyslexia_risk — these groups experience decoding failure reduction that is large, reliable, and not achievable through alternative methods at scale. Victims declared: teachers_professional_judgment — scripted programs remove discretion over instructional sequence, pacing, and responsive adaptation. The directionality derivation: students are near the beneficiary end (d ~ 0.15) because the constraint subsidizes their decoding success; teachers are near the target end (d ~ 0.85) because the constraint extracts their professional discretion. Curriculum publishers are indirect beneficiaries (d ~ 0.25) via market capture but are not declared as primary beneficiaries because they do not shape the constraint's design. State agencies are agenda_setters with analytical exit — they can revise mandates but face political costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ensuring all students, especially those with weak phonological awareness, acquire decoding skills) remains live — decoding failure rates persist in mixed-methods instruction. However, the constraint has accumulated extraction on teacher autonomy that exceeds what the founding problem requires: scripted fidelity mandates go beyond systematic sequence into prescriptive pacing that serves publisher revenue and accountability metrics, not student learning. This is mandatrophy — the mandate has outgrown its function. The constraint resists revision because the interpretation layer (state implementation guides, fidelity rubrics) absorbs counter-evidence rather than transmitting it upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (literacy_acquisition_kernel) rather than an independent constraint?',
    'Cross-constraint analysis of sibling readings (whole_language_reading, balanced_literacy_reading, structured_literacy_reading) to confirm shared referent and structural delta pattern',
    'If confirmed, classification of this reading is bound to the kernel''s commitment structure — drift in one reading''s axioms or authority grounding affects the family',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this story instantiates the phonics_reading of literacy_acquisition_kernel').

omega_variable(
    teacher_autonomy_extraction_boundary,
    'Does scripted phonics instruction extract from teacher professional judgment, or does it scaffold it?',
    'Longitudinal teacher surveys and classroom observation comparing scripted vs. adaptive phonics implementations on teacher retention, sense of efficacy, and instructional responsiveness',
    'If extraction, this reading imposes a structural cost on the teaching profession that balanced_literacy_reading and whole_language_reading avoid; if scaffolding, the cost is transitional',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_extraction_boundary, empirical, 'Whether high extractiveness on teacher autonomy is structural or implementation-dependent').

omega_variable(
    student_decoding_failure_vs_coordination,
    'Is the low extractiveness on student decoding failure a genuine coordination achievement or an artifact of selection effects?',
    'RCT or quasi-experimental comparison of decoding failure rates in systematic phonics vs. mixed-methods instruction controlling for student demographics, dosage, and teacher fidelity',
    'If genuine coordination, the tangled_rope classification is warranted (real benefit + real cost); if artifact, the constraint may be snare or piton',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_decoding_failure_vs_coordination, empirical, 'Whether the claimed student benefit is structurally real or confounded').

omega_variable(
    cs_framing_underdetermination,
    'Does the phonics_reading instantiate a commitment system grounded in a formalized kernel (research consensus), or is its authority extraction-based (institutional mandates preventing revision)?',
    'Trace the policy pathway from research consensus (National Reading Panel, IES practice guides) to state mandates — does the mandate absorb counter-evidence via interpretation layer, or is revision structurally blocked?',
    'If authority_grounding=extraction, the constraint''s persistence depends on suppressing revision; if expertise/lineage, it absorbs drift through normal scientific updating',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the commitment system classification is expertise/lineage or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__phonics_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(lite_tr_t5, observed).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(lite_tr_t10, observed).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__phonics_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(lite_tr_t15, observed).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(lite_tr_t20, observed).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__phonics_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(lite_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement_basis(lite_be_t5, observed).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement_basis(lite_be_t10, observed).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement_basis(lite_be_t15, observed).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(lite_be_t20, observed).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(lite_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement_basis(lite_su_t5, observed).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement_basis(lite_su_t10, observed).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement_basis(lite_su_t15, observed).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement_basis(lite_su_t20, observed).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement_basis(lite_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, information_standard).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__phonics_reading, 0.05).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the literacy_acquisition_kernel constraint family. All four readings share the referent (how children acquire reading) but instantiate different constraints with different ε values, beneficiary/victim structures, and authority groundings. The phonics_reading has higher extractiveness on teacher autonomy than balanced_literacy_reading but lower than structured_literacy_reading (which extends systematicity to more components). The whole_language_reading has near-zero extractiveness on teacher autonomy but high extractiveness on students_with_weak_phonological_awareness (decoding failure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__phonics_reading, organized, 0.85).
constraint_indexing:directionality_override(literacy_acquisition_kernel__phonics_reading, moderate, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
