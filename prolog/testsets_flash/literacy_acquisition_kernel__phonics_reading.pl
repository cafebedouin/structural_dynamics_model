% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Phonics-First Reading Acquisition Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of literacy
 *   acquisition, asserting that explicit, systematic instruction in
 *   phoneme-grapheme correspondence is a prerequisite for reading
 *   comprehension. It mandates a specific instructional sequence,
 *   prioritizing decoding skills before exposure to connected text. This
 *   approach is often codified in curriculum standards and teacher training,
 *   leading to highly structured, sometimes scripted, lesson plans. It is one
 *   of several competing pedagogical approaches within the broader
 *   'literacy_acquisition_kernel' contest.
 *
 * KEY AGENTS:
 *   - curriculum_publishers_phonics_programs: Agenda setter (institutional/arbitrage) — designs and markets phonics-first curricula.
 *   - educational_policymakers: Agenda setter (institutional/analytical) — mandates phonics-first approaches in standards and funding.
 *   - teachers_professional_judgment: Payer (moderate/constrained) — bears the cost of reduced autonomy and scripted instruction.
 *   - students_with_weak_phonological_awareness: Beneficiary (powerless/trapped) — benefits from systematic instruction that addresses their specific learning needs.
 *   - students_with_strong_phonological_awareness: Victim (powerless/trapped) — may be over-constrained by a rigid phonics-first approach that is redundant for them.
 *   - parents_advocating_for_phonics: Beneficiary (organized/mobile) — supports and advocates for phonics-first methods, often based on personal experience or research interpretation.
 *   - whole_language_advocates: Excluded (organized/constrained) — actively opposes the phonics-first mandate, arguing for holistic, meaning-based instruction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.65).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Reading Acquisition Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '00568ac8-4841-4bcb-9aa2-54280774f6ae').
narrative_ontology:cs_kernel_codification('00568ac8-4841-4bcb-9aa2-54280774f6ae', formalized).
narrative_ontology:cs_authority_grounding('00568ac8-4841-4bcb-9aa2-54280774f6ae', expertise).
narrative_ontology:cs_interpretation_layer_present('00568ac8-4841-4bcb-9aa2-54280774f6ae').
narrative_ontology:cs_reading_relation('00568ac8-4841-4bcb-9aa2-54280774f6ae', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('00568ac8-4841-4bcb-9aa2-54280774f6ae', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('00568ac8-4841-4bcb-9aa2-54280774f6ae', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('00568ac8-4841-4bcb-9aa2-54280774f6ae', foundational, decoding_is_primary_gateway_to_comprehension).
narrative_ontology:cs_axiom_status(decoding_is_primary_gateway_to_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('00568ac8-4841-4bcb-9aa2-54280774f6ae', decoding_is_primary_gateway_to_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('00568ac8-4841-4bcb-9aa2-54280774f6ae', foundational, explicit_systematic_phonics_is_essential).
narrative_ontology:cs_axiom_status(explicit_systematic_phonics_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('00568ac8-4841-4bcb-9aa2-54280774f6ae', explicit_systematic_phonics_is_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('00568ac8-4841-4bcb-9aa2-54280774f6ae', scientific_reading_research_consensus).
narrative_ontology:cs_drift_state('00568ac8-4841-4bcb-9aa2-54280774f6ae', contemporary_pedagogical_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('00568ac8-4841-4bcb-9aa2-54280774f6ae', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_phonics_programs).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, students_with_strong_phonological_awareness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, research-backed instructional sequence for teaching reading, ensuring that foundational decoding skills are explicitly taught to all students, particularly benefiting those who do not acquire these skills implicitly.
% TRANSFER_FUNCTION: Transfers instructional control and pedagogical decision-making from individual teachers to curriculum designers and policymakers, in exchange for a perceived reduction in student reading failure rates and increased accountability.
% ABSENT_VOICES: Advocates for 'whole language' and 'balanced literacy' approaches are often marginalized in policy debates dominated by phonics-first proponents. They would argue for greater emphasis on meaning-making, motivation, and integrated literacy experiences, rather than a rigid, decoding-first sequence.
% DISAPPEARANCE_RATIONALE: If the phonics-first mandate vanished, instructional practices would immediately diversify, with many teachers reverting to more integrated or 'balanced' approaches. Curriculum markets would shift, and the political landscape of literacy education would be fundamentally altered, leading to a significant reorganization of pedagogical norms and resource allocation.
% FOUNDING_PROBLEM: A perceived crisis in reading achievement, particularly for students from disadvantaged backgrounds or with learning disabilities, attributed to insufficient or unsystematic phonics instruction in prior pedagogical models.
% FOUNDING_PROBLEM_CORROBORATION: The problem of reading achievement gaps is widely attested by educational researchers, cognitive scientists, and parent advocacy groups (e.g., for dyslexia). While the specific solution (phonics-first mandate) is debated, the underlying problem of ensuring universal literacy remains a live concern, corroborated by national and international assessment data from outside the direct beneficiaries of phonics curricula.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates (systematic instruction for struggling readers) but also extracts (from teacher autonomy and potentially from advanced learners). Extractiveness is high (0.65) due to the rigid curriculum mandates and the suppression of alternative pedagogical approaches. Suppression is also high (0.75) as teachers are often compelled to follow prescribed methods, with professional consequences for deviation. Theater ratio is low (0.1) because the instruction is genuinely delivered, even if its universal applicability is contested; it's not primarily performative. The rising extractiveness and suppression over time reflect the increasing institutionalization and enforcement of phonics-first mandates in many educational systems.
 *
 * PERSPECTIVAL GAP:
 *   Teachers experience this as a highly extractive constraint on their professional judgment, while students with specific learning needs (e.g., dyslexia) may experience it as a beneficial coordination mechanism. Policymakers and curriculum publishers view it as a necessary standard for effective literacy instruction. The engine's per-seat classification will reflect these divergent experiences based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Curriculum publishers and educational policymakers are clear beneficiaries and agenda-setters (low d) as they profit from or enforce the phonics-first mandate. Teachers and some students are victims (high d) as their autonomy or learning preferences are suppressed. Parents advocating for phonics are beneficiaries (low d) as their preferred method is implemented. Whole language advocates are excluded (high d) as their approach is actively suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring all students learn to read) is still live, but its specific implementation (phonics-first as a universal mandate) is contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring the extraction from teachers) or a Snare (ignoring the genuine benefits for some students). The ongoing debate about its efficacy and scope suggests it's not a Piton, as there are active beneficiaries and victims, and significant resources are invested in its maintenance and enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, independent claim, or one reading of the ''literacy_acquisition_kernel''?',
    'The presence of ''literacy_acquisition_kernel'' in the kernel_context and cs_structure confirms it is a reading.',
    'If a reading, its classification is understood in relation to sibling readings and the overall kernel contest; if independent, it stands alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''phonics_reading'' of the ''literacy_acquisition_kernel''.').

omega_variable(
    teacher_autonomy_vs_student_outcomes,
    'Does the high extractiveness on teacher autonomy (via scripted phonics lessons) genuinely lead to improved student decoding outcomes, or is it an overreach?',
    'Longitudinal studies comparing student outcomes (decoding, fluency, comprehension) in classrooms with high-fidelity phonics implementation vs. those with greater teacher autonomy in instructional method, controlling for student demographics.',
    'If outcomes are significantly better, the extractiveness is a necessary cost of coordination; if not, it''s pure extraction from teachers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_student_outcomes, empirical, 'Assesses the functional justification for extracting teacher autonomy.').

omega_variable(
    phonics_scope_for_all_learners,
    'Is explicit, systematic phonics instruction equally beneficial for all students, or does it over-constrain those who acquire decoding skills more readily?',
    'Differential impact studies on student groups with varying initial phonological awareness and language backgrounds. Analysis of engagement and motivation metrics for students who find phonics instruction redundant.',
    'If universally beneficial, the constraint is a broad coordination mechanism; if differentially beneficial, it extracts from some students (e.g., boredom, reduced motivation) while benefiting others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_scope_for_all_learners, empirical, 'Evaluates the universal applicability and potential over-prescription of phonics-first instruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__phonics_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__phonics_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
