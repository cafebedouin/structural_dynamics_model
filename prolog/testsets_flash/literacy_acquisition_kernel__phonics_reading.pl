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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Phonics-First Reading Acquisition Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of reading
 *   acquisition, which mandates explicit, systematic instruction in
 *   phoneme-grapheme correspondence as a prerequisite for connected text
 *   exposure. It is a reading of the broader 'literacy_acquisition_kernel'
 *   and stands in contrast to other pedagogical approaches. While it aims to
 *   coordinate effective instruction, it also extracts from teacher autonomy
 *   and potentially from students for whom this approach is redundant. The
 *   metrics reflect a system that has become increasingly prescriptive and
 *   enforced over time, driven by research findings and policy mandates.
 *
 * KEY AGENTS:
 *   - teachers_professional_judgment: Payer (moderate/identity_locked) — bears extraction of autonomy
 *   - students_with_weak_phonological_awareness: Beneficiary (powerless/trapped) — benefits from explicit instruction
 *   - curriculum_publishers_phonics_programs: Beneficiary (organized/arbitrage) — profits from mandates
 *   - students_with_strong_phonological_awareness: Payer (powerless/trapped) — bears cost of redundant instruction
 *   - school_administrators: Agenda-setter (institutional/constrained) — enforces mandates
 *   - cognitive_science_researchers: Observer (analytical/analytical) — provides evidence, influences policy
 *   - whole_language_advocates: Excluded (moderate/constrained) — marginalized by mandate
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
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Reading Acquisition Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, 'e49c658b-31ae-4ad7-8b06-255d87068a60').
narrative_ontology:cs_kernel_codification('e49c658b-31ae-4ad7-8b06-255d87068a60', formalized).
narrative_ontology:cs_authority_grounding('e49c658b-31ae-4ad7-8b06-255d87068a60', expertise).
narrative_ontology:cs_interpretation_layer_present('e49c658b-31ae-4ad7-8b06-255d87068a60').
narrative_ontology:cs_reading_relation('e49c658b-31ae-4ad7-8b06-255d87068a60', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('e49c658b-31ae-4ad7-8b06-255d87068a60', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('e49c658b-31ae-4ad7-8b06-255d87068a60', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('e49c658b-31ae-4ad7-8b06-255d87068a60', foundational, decoding_is_primary).
narrative_ontology:cs_axiom_status(decoding_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('e49c658b-31ae-4ad7-8b06-255d87068a60', decoding_is_primary, empirically_contingent).
narrative_ontology:cs_axiom('e49c658b-31ae-4ad7-8b06-255d87068a60', foundational, explicit_instruction_is_necessary).
narrative_ontology:cs_axiom_status(explicit_instruction_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e49c658b-31ae-4ad7-8b06-255d87068a60', explicit_instruction_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('e49c658b-31ae-4ad7-8b06-255d87068a60', science_of_reading_consensus).
narrative_ontology:cs_drift_state('e49c658b-31ae-4ad7-8b06-255d87068a60', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e49c658b-31ae-4ad7-8b06-255d87068a60', '').
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

% Teachers are often required to follow highly scripted phonics curricula, limiting their autonomy to adapt instruction based on student needs or their own pedagogical expertise. This can lead to professional dissatisfaction and a feeling of de-skilling.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment, payer,
    moderate, biographical, identity_locked, local).

% These students benefit significantly from explicit, systematic phonics instruction, which provides them with foundational decoding skills necessary for reading. Without it, they are at high risk of reading failure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, immediate, trapped, local).

% These publishers profit from the widespread adoption of phonics-first mandates, selling extensive, often scripted, instructional materials to school districts. They actively advocate for policies that reinforce this approach.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_phonics_programs, beneficiary,
    organized, generational, arbitrage, national).

% For these students, highly explicit and systematic phonics instruction can be redundant, boring, and may delay their engagement with rich, connected texts, potentially dampening reading motivation and comprehension development.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_strong_phonological_awareness, payer,
    powerless, immediate, trapped, local).

% Implement and enforce phonics-first mandates, often driven by state-level policies, accountability metrics, and pressure from parent advocacy groups. They balance pedagogical effectiveness with political and financial considerations.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, school_administrators, agenda_setter,
    institutional, biographical, constrained, local).

% Conduct studies on reading acquisition, providing empirical evidence that often supports the importance of phonics. Their findings influence policy but do not directly set mandates.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, cognitive_science_researchers, observer,
    analytical, generational, analytical, global).

% Advocate for approaches that prioritize meaning-making and engagement with authentic texts, often viewing explicit phonics as secondary or even detrimental. Their pedagogical philosophy is often marginalized or actively suppressed by phonics-first mandates.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates instructional practice across classrooms and schools to ensure a consistent, research-backed approach to teaching foundational reading skills, particularly decoding, to all students.
% TRANSFER_FUNCTION: Transfers instructional control and pedagogical decision-making from individual teachers to prescribed curricula and mandates, while transferring foundational decoding skills to students, especially those at risk.
% ABSENT_VOICES: Advocates for whole language or purely emergent literacy approaches are largely excluded from policy-making and curriculum selection processes, as their core tenets are seen as antithetical to phonics-first mandates. Their voices would highlight potential negative impacts on reading motivation and the over-simplification of reading as a cognitive process.
% DISAPPEARANCE_RATIONALE: If the phonics-first mandate vanished overnight, many schools would revert to more eclectic or 'balanced' approaches, curriculum publishers would lose a significant market, and the instructional landscape for early literacy would become highly varied, potentially leading to inconsistent outcomes for students, especially those needing explicit support.
% FOUNDING_PROBLEM: A significant portion of students, particularly those from disadvantaged backgrounds or with specific learning differences, were failing to acquire basic decoding skills, leading to widespread reading failure and educational inequity.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science research consistently identifies phonological awareness and decoding as critical for reading acquisition, especially for struggling readers. Educational outcomes data continue to show disparities in reading proficiency, corroborating the ongoing need for effective foundational instruction. This is attested by independent researchers and educational advocacy groups, not just curriculum providers.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is high due to the significant loss of teacher autonomy and the potential for redundant instruction for some students. Suppression (0.75) is also high, reflecting the active enforcement of mandates and the marginalization of alternative pedagogies. Theater ratio is low (0.1) because the instruction is genuinely intended to be effective, though its implementation may sometimes be performative rather than truly responsive. The increasing extractiveness and suppression over time reflect the 'reading wars' and the hardening of policy around phonics-first approaches.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of students with weak phonological awareness, this constraint is a clear Rope or even a Scaffold, providing essential support. From the perspective of teachers whose professional judgment is overridden, or students for whom the instruction is redundant, it functions more as a Snare or Tangled Rope. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'os_marketplace_operator' (school_administrators and curriculum_publishers) benefits from the standardization and sale of curricula. 'Major_app_publishers' (teachers) bear the cost of reduced autonomy. 'Device_users' (students) are split: those with weak phonological awareness are beneficiaries, while those with strong awareness are payers of redundant instruction. 'Rival_payment_networks' (whole_language_advocates) are excluded from the pedagogical discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (addressing reading failure) is still live, but its implementation has become increasingly prescriptive, leading to concerns about over-extraction from teacher autonomy and student engagement. The classification as Tangled Rope acknowledges both the genuine coordination function (teaching foundational skills) and the asymmetric extraction (from teachers and some students). The omegas address the ongoing contestation of its scope and necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    teacher_autonomy_vs_student_outcomes,
    'Does the prescriptive nature of phonics-first mandates, by reducing teacher autonomy, ultimately hinder overall student reading development (e.g., comprehension, motivation) even as decoding skills improve?',
    'Longitudinal studies comparing student outcomes (decoding, comprehension, motivation) in highly scripted phonics environments versus contexts where teachers have greater pedagogical flexibility within a phonics-informed framework.',
    'If reduced autonomy negatively impacts broader outcomes, the effective extraction from teachers is higher, and the constraint''s overall benefit as a coordination mechanism is reduced, pushing it closer to a Snare. If outcomes are superior, the extraction is justified as a necessary cost of effective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_student_outcomes, empirical, 'Trade-off between instructional fidelity and teacher responsiveness.').

omega_variable(
    scope_of_phonics_necessity,
    'Is explicit, systematic phonics instruction equally necessary and beneficial for ALL students, or is its primary benefit concentrated among students with specific learning challenges or weaker phonological awareness?',
    'Meta-analyses of intervention studies that disaggregate effects by student baseline phonological awareness and prior reading ability. Neuroimaging studies identifying differential neural pathways activated by various instructional approaches.',
    'If the benefit is highly concentrated, the constraint''s universal application extracts unnecessarily from students who would thrive with less explicit instruction, increasing its effective extractiveness for that group and strengthening the Snare-like qualities. If universally beneficial, the coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_phonics_necessity, empirical, 'Whether phonics instruction should be universally applied or differentiated.').

omega_variable(
    reading_as_decoding_vs_meaning_making,
    'Is reading fundamentally a decoding process that enables comprehension, or is it a meaning-making process where decoding is one of several cues?',
    'This is a conceptual and theoretical debate within cognitive science and education, unlikely to be resolved by a single empirical finding. Resolution would require a paradigm shift in how reading is defined and understood.',
    'If reading is primarily meaning-making, the phonics-first mandate over-emphasizes a sub-component, making its coordination function less central and its extraction more pronounced. If decoding is primary, the mandate is well-aligned with the nature of reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_as_decoding_vs_meaning_making, conceptual, 'Fundamental definition of reading process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1980, literacy_acquisition_kernel__phonics_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__phonics_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel'. Its structural properties and metrics reflect the specific claims and impacts of a phonics-first approach, distinct from other pedagogical readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
