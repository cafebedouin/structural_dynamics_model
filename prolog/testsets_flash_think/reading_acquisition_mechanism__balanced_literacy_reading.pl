% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Approach to Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'balanced literacy' approach to reading
 *   acquisition, which posits that both explicit phonics instruction and
 *   authentic literature exposure are necessary in integrated practice. It is
 *   one reading of the broader 'reading_acquisition_mechanism' kernel, which
 *   is highly contested. While presented as a balanced coordination
 *   mechanism, its variable implementation fidelity often leads to
 *   insufficient systematic phonics, resulting in extraction from students
 *   who struggle with decoding. It functions as an institutional compromise
 *   position, often collapsing towards whole-language practices despite its
 *   stated intent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.65).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.7).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Approach to Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, 'a6849caa-9a9e-4944-ba89-3bd7b3cf509b').
narrative_ontology:cs_kernel_codification('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', formalized).
narrative_ontology:cs_authority_grounding('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', practice).
narrative_ontology:cs_interpretation_layer_present('a6849caa-9a9e-4944-ba89-3bd7b3cf509b').
narrative_ontology:cs_reading_relation('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', reading_acquisition_mechanism__whole_language_reading, influences).
narrative_ontology:cs_axiom('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', foundational, reading_is_a_natural_process_with_explicit_components).
narrative_ontology:cs_axiom_status(reading_is_a_natural_process_with_explicit_components, holdable).
narrative_ontology:cs_axiom_grounding('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', reading_is_a_natural_process_with_explicit_components, conventional).
narrative_ontology:cs_axiom('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', foundational, integrated_instruction_is_optimal).
narrative_ontology:cs_axiom_status(integrated_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', integrated_instruction_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', integrated_holistic_literacy_development).
narrative_ontology:cs_drift_state('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', contemporary_reading_wars_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6849caa-9a9e-4944-ba89-3bd7b3cf509b', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_developers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, school_administrators).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, students_struggling_with_decoding).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, teachers_lacking_phonics_training).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, teachers_implementing_balanced_literacy).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, advocates_for_science_of_reading).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, holistic_learning_theory).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, developmental_appropriateness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing and enforcing curriculum mandates, often adopting balanced literacy as a compromise to avoid 'reading wars'. They benefit from perceived pedagogical unity but bear the costs of managing implementation challenges and public critique.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, school_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Profit from selling curriculum materials, textbooks, and professional development programs aligned with balanced literacy frameworks. They have a strong incentive to maintain the approach's market viability.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers, beneficiary,
    organized, biographical, mobile, global).

% Design and promote balanced literacy frameworks, often holding influential positions in educational policy and teacher training. They benefit from the widespread adoption of their pedagogical models.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_developers, beneficiary,
    organized, biographical, constrained, national).

% Are on the front lines of instruction, often struggling with the fidelity of implementing both explicit phonics and authentic literature exposure. They bear the burden of inconsistent training and pressure to achieve results with a potentially flawed model.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teachers_implementing_balanced_literacy, payer,
    moderate, biographical, constrained, local).

% Are directly harmed by insufficient or unsystematic phonics instruction within balanced literacy, leading to difficulties in foundational reading skills. Their educational trajectory is significantly impacted.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, students_struggling_with_decoding, payer,
    powerless, immediate, trapped, local).

% Study reading acquisition from a scientific perspective, often critiquing balanced literacy for its lack of systematic phonics and advocating for evidence-based practices (Science of Reading). They observe and analyze the constraint's effects.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, cognitive_scientists_literacy_researchers, observer,
    analytical, generational, analytical, global).

% Often lack the pedagogical expertise or institutional voice to effectively challenge the balanced literacy approach, even when their children are demonstrably failing to learn to read. They are outside the core decision-making loop.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, parents_of_struggling_readers, excluded,
    moderate, biographical, constrained, local).

% Actively resist the balanced literacy approach, advocating for explicit, systematic phonics instruction based on cognitive science research. They bear the costs of organizing, lobbying, and educating the public against the prevailing pedagogical paradigm.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, advocates_for_science_of_reading, payer,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To integrate explicit phonics instruction with meaningful engagement with authentic literature, aiming to provide a comprehensive and balanced approach to reading acquisition that addresses both decoding and comprehension skills.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design to proponents of balanced literacy, and financial resources to educational publishers. It transfers varied learning outcomes (often suboptimal for some students) to the student population.
% ABSENT_VOICES: Advocates for explicit, systematic phonics (including many cognitive scientists and parents of children with dyslexia) are often marginalized in policy discussions, which are frequently dominated by educational institutions and publishers promoting balanced literacy. Their concerns about insufficient phonics are often dismissed as 'extremist' or 'unbalanced'.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, there would be a significant and immediate reorganization of curriculum, teacher training, and educational policy. Schools would likely shift towards either more explicit phonics-based instruction or a return to whole language, leading to a complete overhaul of literacy education materials and professional development.
% FOUNDING_PROBLEM: The 'reading wars' between proponents of whole language (focus on meaning, implicit learning) and phonics (focus on decoding, explicit instruction) created a deep division in literacy pedagogy. Balanced literacy emerged as an institutional compromise to reconcile these opposing views and unify pedagogical practice.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (curriculum developers, some administrators) argue the problem of integrating diverse reading skills is still live. Critics (cognitive scientists, advocates for science of reading) attest that while the initial problem of division was real, the compromise has failed to adequately address foundational decoding skills for all learners, and the arrangement persists more as an institutional peace treaty than an effective solution. Legislative hearings and independent research from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the 'balance' often dilutes systematic phonics, leading to poor outcomes for many students, particularly those with dyslexia or other learning differences. Suppression (0.70) is significant due to curriculum mandates, teacher training that may de-emphasize phonics, and the marginalization of alternative pedagogical voices. The theater ratio (0.45) reflects the gap between the stated 'balanced' ideal and the often-inconsistent or whole-language-leaning practice, where the label is maintained for institutional peace. Accessibility collapse (0.60) is moderate as alternatives are known but often suppressed by institutional inertia. Resistance (0.55) is ongoing, fueled by the 'Science of Reading' movement. The measurement series shows a gradual increase in extractiveness and theater as the approach's implementation challenges became more apparent over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of school administrators and curriculum developers, balanced literacy is a necessary and effective compromise that coordinates diverse pedagogical needs. From the perspective of students struggling with decoding and their parents, it is an extractive system that fails to provide essential foundational skills. Teachers often experience it as a constrained mandate, caught between theoretical ideals and practical realities.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational publishers and curriculum developers are clear beneficiaries (low d) as they profit from the widespread adoption of balanced literacy materials and frameworks. School administrators benefit from the perceived resolution of pedagogical conflicts. Students struggling with decoding are primary victims (high d) due to the direct impact on their learning. Teachers, while part of the system, often bear the costs of implementation challenges and inadequate training, making them payers. Advocates for the Science of Reading are also payers, expending resources to challenge the status quo. Parents of struggling readers are excluded, lacking direct influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate for 'balance' has, for many, outlived its original function of genuinely integrating effective phonics and literature exposure. Instead, it often serves as an institutional mechanism to avoid conflict and maintain a market for specific curriculum products, even when its pedagogical effectiveness is compromised. The persistence is due to institutional inertia and the difficulty of shifting large educational systems, rather than universal efficacy. This makes it a Tangled Rope, where the coordination story covers an increasingly extractive reality for a significant portion of the student population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_ambiguity,
    'Is the observed ineffectiveness of balanced literacy due to flaws in its theoretical framework, or primarily due to inconsistent and inadequate implementation in practice?',
    'Rigorous, large-scale studies comparing student outcomes in classrooms with high-fidelity balanced literacy implementation versus those with low-fidelity implementation, and against alternative evidence-based approaches.',
    'If theoretical flaws are primary, the constraint is more inherently extractive (Snare-like). If implementation is the main issue, it suggests a potential Rope that is poorly executed, and interventions should focus on teacher training and support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_ambiguity, empirical, 'Distinguishing between theoretical flaws and implementation gaps in balanced literacy.').

omega_variable(
    phonics_sufficiency_ambiguity,
    'Is the ''explicit phonics'' component within balanced literacy genuinely sufficient for all learners, or is it systematically diluted or de-emphasized in a way that makes it insufficient for many?',
    'Content analysis of widely adopted balanced literacy curricula and teacher training materials, combined with classroom observation studies, to quantify the systematicity and explicitness of phonics instruction provided.',
    'If phonics is systematically diluted, the constraint''s extractiveness is higher than claimed, particularly for vulnerable learners, pushing it closer to a Snare. If it is genuinely sufficient but poorly taught, it points to a training problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_sufficiency_ambiguity, empirical, 'Assessing the actual sufficiency of phonics instruction within balanced literacy.').

omega_variable(
    institutional_compromise_vs_effectiveness,
    'Is the persistence of balanced literacy primarily due to its pedagogical effectiveness for the majority of students, or its function as an institutional compromise that maintains peace among educational factions and benefits specific commercial interests?',
    'Analysis of policy documents, funding flows to publishers, and longitudinal student outcome data, alongside qualitative studies of educator decision-making processes and political pressures within school systems.',
    'If institutional compromise and commercial interests are the primary drivers, the constraint is more extractive and theatrical (Tangled Rope or Piton), with its coordination story serving as a cover. If effectiveness is primary, it is closer to a genuine Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_compromise_vs_effectiveness, conceptual, 'Understanding the true drivers of balanced literacy''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(read_tr_t6, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(read_tr_t18, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 18, 0.43).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(read_be_t6, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(read_be_t12, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(read_be_t18, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(read_su_t6, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(read_su_t12, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(read_su_t18, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_curriculum).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, standardized_reading_assessments).

% DUAL FORMULATION NOTE:
% This constraint is the 'balanced_literacy_reading' of the 'reading_acquisition_mechanism' kernel. It coexists with the 'phonics_reading' and influences the 'whole_language_reading' by having emerged from it and often drifting back towards its practices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
