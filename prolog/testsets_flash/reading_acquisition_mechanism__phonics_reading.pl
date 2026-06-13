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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Phonics-First Reading Instruction Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of the reading
 *   acquisition mechanism, asserting that explicit, systematic instruction in
 *   grapheme-phoneme correspondence is a foundational and necessary skill for
 *   reading acquisition. It is a pedagogical approach that has gained
 *   significant traction in educational policy, often mandated by state or
 *   national curricula. The constraint implies a specific instructional
 *   sequence and resource allocation, prioritizing decoding skills over other
 *   aspects of literacy in early stages.
 *
 * KEY AGENTS:
 *   - struggling_readers: Primary beneficiary (powerless/immediate) — disproportionately benefits from explicit instruction.
 *   - curriculum_publishers_phonics: Primary beneficiary (organized/biographical) — profits from demand for phonics-based materials.
 *   - cognitive_science_researchers: Beneficiary (analytical/generational) — their research is vindicated and funded.
 *   - teachers_whole_language_trained: Primary victim (moderate/biographical) — faces retraining costs, loss of pedagogical autonomy.
 *   - curriculum_publishers_whole_language: Victim (organized/biographical) — loses market share.
 *   - educational_policymakers: Agenda setter (institutional/generational) — mandates and enforces the curriculum.
 *   - parents: Payer/Beneficiary (moderate/biographical) — bears costs of specific materials, but benefits from perceived effectiveness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.4).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.6).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Phonics-First Reading Instruction Mandate").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, 'd41c6dd1-2f31-43f9-815f-b3d66296e7d6').
narrative_ontology:cs_kernel_codification('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', formalized).
narrative_ontology:cs_authority_grounding('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', expertise).
narrative_ontology:cs_interpretation_layer_present('d41c6dd1-2f31-43f9-815f-b3d66296e7d6').
narrative_ontology:cs_reading_relation('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', foundational, alphabetic_principle_primacy).
narrative_ontology:cs_axiom_status(alphabetic_principle_primacy, holdable).
narrative_ontology:cs_axiom_grounding('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', alphabetic_principle_primacy, empirically_contingent).
narrative_ontology:cs_axiom('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', foundational, explicit_instruction_necessity).
narrative_ontology:cs_axiom_status(explicit_instruction_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', explicit_instruction_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', science_of_reading_consensus).
narrative_ontology:cs_drift_state('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', contemporary_pedagogical_practice, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d41c6dd1-2f31-43f9-815f-b3d66296e7d6', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, curriculum_publishers_phonics).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, cognitive_science_researchers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers_whole_language_trained).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, curriculum_publishers_whole_language).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, science_of_reading_doctrine).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, alphabetic_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, explicit method for teaching foundational decoding skills, ensuring that all students, particularly those at risk, receive systematic instruction in how written language maps to spoken sounds.
% TRANSFER_FUNCTION: Transfers instructional resources (teacher training, curriculum materials, assessment focus) towards explicit phonics instruction, and transfers pedagogical authority from individual teachers to evidence-based mandates.
% ABSENT_VOICES: Advocates for 'whole language' or 'balanced literacy' approaches, who emphasize meaning-making and authentic text engagement from the outset, are often marginalized in policy debates and curriculum adoption processes, despite their continued presence in academic discourse and some classrooms.
% DISAPPEARANCE_RATIONALE: If the mandate for explicit systematic phonics instruction vanished overnight, there would be a rapid shift in curriculum development, teacher training, and classroom practice. Many teachers would revert to more eclectic or 'balanced' approaches, and the market for educational materials would diversify. This would lead to a reorganization of pedagogical norms and resource allocation in literacy education.
% FOUNDING_PROBLEM: A significant portion of the student population, particularly those from disadvantaged backgrounds or with learning disabilities, struggled to acquire reading skills effectively through implicit or less structured methods.
% FOUNDING_PROBLEM_CORROBORATION: The problem of reading failure remains a persistent concern, attested by ongoing national and international literacy assessments (e.g., NAEP, PISA) and by advocacy groups for children with dyslexia. While the effectiveness of phonics is debated, the existence of a significant population struggling with reading is widely corroborated by independent educational researchers and public health data, not just by proponents of phonics.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).
:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the cost of specialized training for teachers and the exclusion of alternative methods, but also the genuine benefit to many learners. Suppression (0.6) is significant due to top-down mandates, curriculum standardization, and the marginalization of non-phonics approaches. Theater ratio (0.1) is low, as the instruction is genuinely implemented and its effects are measurable, though contested. Accessibility collapse (0.7) is high because once this method is adopted, alternatives are often removed from the instructional environment. Resistance (0.3) is moderate, coming from educators and researchers advocating for broader literacy approaches.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of struggling readers and cognitive science researchers, this constraint is a highly effective Rope, providing a necessary structure for literacy. From the perspective of teachers trained in whole language or balanced literacy, it can feel like a Snare, imposing a method that conflicts with their training and professional judgment, and limiting their autonomy. Educational policymakers often view it as a necessary coordination mechanism to ensure equitable literacy outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers are clear beneficiaries (d=0.0) as explicit phonics often provides the scaffolding they need. Phonics curriculum publishers also benefit (d=0.1) from increased demand. Teachers trained in whole language (d=0.9) bear costs through retraining and loss of autonomy. Educational policymakers (d=0.2) benefit from a clear, defensible policy, but also bear the political costs of implementation. Parents are mixed (d=0.5), benefiting from perceived effectiveness but potentially paying for specific materials.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is currently live, driven by ongoing debates about literacy rates and the 'science of reading.' Mandatrophy is prevented by the continuous empirical validation and advocacy from cognitive science, which keeps the 'founding problem' (ineffective reading instruction) in the 'live' status. However, if future research were to show that a phonics-exclusive approach leads to long-term deficits in comprehension or reading enjoyment, the constraint could drift towards mandatrophy, as its core justification would erode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a universal principle of reading acquisition, or one reading of a contested pedagogical kernel?',
    'Empirical evidence from longitudinal studies comparing outcomes across different instructional methods in diverse populations, particularly for advanced comprehension and reading enjoyment.',
    'If a universal principle, its extractiveness is a necessary cost of effective instruction. If a contested reading, its extractiveness (e.g., from suppressing alternative methods) is a policy choice with identifiable beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''phonics_reading'' of the ''reading_acquisition_mechanism'' kernel. Sibling readings (''whole_language_reading'', ''balanced_literacy_reading'') would shift the beneficiary/victim structure and perceived extractiveness.').

omega_variable(
    teacher_discretion_vs_fidelity,
    'To what extent does the mandate for systematic phonics instruction genuinely improve outcomes versus merely narrowing teacher discretion and increasing compliance burden?',
    'Studies on teacher efficacy and burnout under strict phonics mandates versus more flexible approaches, controlling for student demographics and prior training.',
    'If the primary effect is reduced discretion without proportional outcome gains, the suppression metric is higher than justified by coordination, indicating a more extractive constraint on teachers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_discretion_vs_fidelity, empirical, 'Assessing the true impact of narrowed teacher discretion under phonics mandates.').

omega_variable(
    long_term_comprehension_impact,
    'Does early, intensive phonics instruction lead to superior long-term reading comprehension and enjoyment, or does it potentially neglect other crucial aspects of literacy development?',
    'Longitudinal studies tracking students from phonics-intensive programs through high school and beyond, assessing not just decoding but also vocabulary, inference, critical analysis, and reading habits.',
    'If long-term comprehension is not significantly improved or is even hindered, the claimed benefits of this constraint are overstated, potentially reclassifying it as a Snare for advanced readers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_comprehension_impact, empirical, 'Evaluating the long-term effects of phonics-first instruction on holistic literacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__phonics_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__phonics_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 10, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('phonics_reading') of the 'reading_acquisition_mechanism' kernel. Its structural properties and classification differ significantly from sibling readings like 'whole_language_reading' and 'balanced_literacy_reading', which emphasize different aspects of literacy acquisition and have distinct beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
