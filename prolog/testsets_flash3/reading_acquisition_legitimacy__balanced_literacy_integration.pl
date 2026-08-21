% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint describes the 'balanced literacy' approach to reading
 *   instruction, which seeks to integrate explicit phonics instruction with
 *   immersion in authentic literature. It emerged as a compromise in the
 *   'reading wars' to address the perceived limitations of purely
 *   phonics-based or whole-language methods. While intended as a 'rope' to
 *   coordinate diverse pedagogical views, its implementation often leads to
 *   uneven outcomes, particularly for struggling readers who may not receive
 *   the intensive, systematic phonics they require. The constraint's
 *   persistence is driven by its appeal as a middle-ground solution, despite
 *   ongoing scientific debate about its efficacy for all learners.
 *
 * KEY AGENTS:
 *   - classroom_teachers: Agenda-setter (organized/constrained) — implement the approach
 *   - mainstream_students: Beneficiary (powerless/trapped) — benefit from varied instruction
 *   - struggling_readers: Payer (powerless/identity_locked) — bear costs of insufficient explicit phonics
 *   - parents_of_struggling_readers: Payer (moderate/constrained) — advocate for more explicit instruction
 *   - literacy_researchers_advocating_structured_literacy: Excluded (powerful/analytical) — argue for 'science of reading'
 *   - curriculum_publishers: Beneficiary (organized/arbitrage) — profit from 'balanced' materials
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.45).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.3).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, '5b93bc4c-1eab-4f89-aa44-0f838d1bea12').
narrative_ontology:cs_kernel_codification('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', formalized).
narrative_ontology:cs_authority_grounding('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', practice).
narrative_ontology:cs_interpretation_layer_present('5b93bc4c-1eab-4f89-aa44-0f838d1bea12').
narrative_ontology:cs_reading_relation('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', foundational, reading_is_both_decoding_and_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_both_decoding_and_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', reading_is_both_decoding_and_meaning_making, empirically_contingent).
narrative_ontology:cs_axiom('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', foundational, instruction_should_be_responsive_to_individual_student_needs).
narrative_ontology:cs_axiom_status(instruction_should_be_responsive_to_individual_student_needs, holdable).
narrative_ontology:cs_axiom_grounding('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', instruction_should_be_responsive_to_individual_student_needs, instrumental).
narrative_ontology:cs_reference_frame('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', post_reading_wars_synthesis).
narrative_ontology:cs_drift_state('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5b93bc4c-1eab-4f89-aa44-0f838d1bea12', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement balanced literacy approaches, integrating explicit phonics with authentic literature. They value the flexibility and responsiveness to individual student needs, but often face pressure from both 'phonics-first' and 'whole-language' advocates.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, agenda_setter,
    organized, biographical, constrained, local).

% Benefit from a varied approach that caters to different learning styles and provides rich literary experiences alongside foundational decoding skills. Their success is often dependent on the teacher's skill in balancing these elements.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_students, beneficiary,
    powerless, immediate, trapped, local).

% May not receive sufficient systematic phonics instruction to develop strong decoding skills, leading to persistent reading difficulties. While they receive some intervention, the integrated approach can dilute the intensity needed for foundational skill gaps.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, biographical, identity_locked, local).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking private tutoring or advocating for more explicit instruction. They feel constrained by the school's chosen pedagogical approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers, payer,
    moderate, generational, constrained, local).

% Argue that balanced literacy often fails struggling readers by not providing sufficiently explicit and systematic phonics, advocating for a 'science of reading' approach. Their research is often cited but not fully integrated into policy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_researchers_advocating_structured_literacy, excluded,
    powerful, generational, analytical, national).

% Develop and sell 'balanced literacy' materials that often include components of both phonics and literature, catering to a broad market and avoiding explicit alignment with more controversial 'camps'.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers, beneficiary,
    organized, biographical, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse pedagogical approaches to reading instruction, aiming to provide a comprehensive framework that addresses both decoding and comprehension, satisfying various educational philosophies and teacher preferences.
% TRANSFER_FUNCTION: Transfers pedagogical flexibility and a broad curriculum to teachers and mainstream students, while potentially transferring the burden of foundational skill gaps and remediation costs to struggling readers and their families.
% ABSENT_VOICES: Advocates for 'structured literacy' and 'phonics-first' approaches, particularly those representing the needs of dyslexic learners, are often marginalized in policy discussions that favor a 'balanced' compromise. They would argue for a stronger emphasis on explicit, systematic phonics.
% DISAPPEARANCE_RATIONALE: If the balanced literacy framework vanished overnight, schools would likely revert to either purely phonics-based or purely whole-language approaches, leading to significant pedagogical shifts, curriculum overhauls, and renewed ideological battles in education policy. Teacher training and resource allocation would be profoundly affected.
% FOUNDING_PROBLEM: The 'reading wars' of the late 20th century created a polarized environment where educators felt forced to choose between phonics (decoding) and whole language (meaning-making), leading to incomplete instructional models and inconsistent student outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Educational policy documents, teacher professional organizations, and many mainstream educators attest that the 'reading wars' problem is still live, requiring a balanced approach. However, a growing body of cognitive science research and advocates for struggling readers (outside the direct beneficiaries) contest that the 'balance' often underserves those most in need of explicit phonics.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) is moderate, reflecting the costs borne by struggling readers who may not acquire foundational decoding skills efficiently, leading to long-term educational disadvantages. Suppression (0.30) is present as alternative, more explicit instructional methods are often marginalized or resisted within mainstream educational institutions. Theater ratio (0.20) is low, as the approach genuinely attempts to provide comprehensive instruction, but some 'balance' can be performative, masking insufficient phonics. The rising extractiveness and suppression over time reflect increasing evidence of the approach's limitations for some learners, leading to greater costs for those underserved and increased pressure to maintain the 'balance' against calls for reform.
 *
 * PERSPECTIVAL GAP:
 *   Classroom teachers and mainstream students often perceive balanced literacy as a beneficial and flexible 'rope' that provides a rich learning environment. In contrast, struggling readers and their parents, along with advocates for structured literacy, experience it as a 'tangled rope' or even a 'snare' due to the insufficient explicit phonics instruction and the resulting academic struggles. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Classroom teachers benefit from the pedagogical flexibility and broad appeal of balanced literacy, placing them closer to the beneficiary end. Mainstream students also benefit from the varied instruction. Struggling readers and their parents bear the primary costs of the approach's limitations, positioning them firmly as targets. Curriculum publishers benefit from selling materials that fit the 'balanced' paradigm. Literacy researchers advocating for structured literacy are excluded, their arguments often suppressed by the prevailing consensus.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling a genuinely intended coordination mechanism (rope) as pure extraction (snare) by acknowledging its initial problem-solving function. However, the rising extractiveness and suppression, coupled with the 'contested' status of the founding problem, suggest a drift towards a 'tangled rope' where the coordination function is increasingly overshadowed by asymmetric costs for vulnerable learners. The 'balance' itself becomes a mechanism for maintaining the status quo against evidence of its limitations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_for_struggling_readers,
    'Does the ''balanced literacy'' approach, as typically implemented, provide sufficient explicit and systematic phonics instruction to ensure reading acquisition for all learners, particularly those with foundational decoding challenges?',
    'Large-scale, longitudinal randomized controlled trials comparing balanced literacy outcomes against structured literacy approaches for diverse student populations, especially those identified as at-risk for reading difficulties.',
    'If found insufficient for struggling readers, the constraint''s extractiveness and suppression would be re-evaluated upward, potentially reclassifying it as a ''tangled rope'' or ''snare'' for those seats, and increasing pressure for policy reform towards more explicit instruction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_for_struggling_readers, empirical, 'Empirical evidence on the effectiveness of balanced literacy for all student populations.').

omega_variable(
    pedagogical_flexibility_vs_fidelity,
    'Is the pedagogical flexibility inherent in ''balanced literacy'' a feature that allows teachers to adapt to student needs, or a bug that leads to inconsistent implementation and insufficient fidelity to evidence-based practices?',
    'Qualitative studies of classroom implementation, examining teacher training, instructional practices, and student outcomes across diverse contexts. Analysis of policy documents for clarity and specificity regarding phonics requirements.',
    'If flexibility leads to inconsistent implementation and poor outcomes, the ''theater_ratio'' would increase, and the ''claimed_type'' as a ''rope'' would be challenged, suggesting a ''piton'' or ''tangled rope'' where the stated coordination function is not reliably delivered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_flexibility_vs_fidelity, conceptual, 'The role of pedagogical flexibility in balanced literacy implementation.').

omega_variable(
    political_compromise_vs_scientific_consensus,
    'To what extent does the persistence of ''balanced literacy'' represent a political compromise in education policy, rather than a pedagogical approach fully aligned with the scientific consensus on reading acquisition?',
    'Analysis of legislative debates, policy documents, and professional organization statements, comparing their justifications for balanced literacy against the evolving ''science of reading'' research base. Expert panel review of policy alignment.',
    'If primarily a political compromise, the ''suppression'' of alternative approaches would be seen as more extractive, and the ''claimed_type'' as a ''rope'' would be challenged, suggesting a ''tangled rope'' or ''snare'' where political expediency overrides evidence-based practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_compromise_vs_scientific_consensus, preference, 'The influence of political compromise versus scientific evidence on literacy policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, resource_allocation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_professional_development_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_adoption_processes).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, special_education_referral_criteria).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
