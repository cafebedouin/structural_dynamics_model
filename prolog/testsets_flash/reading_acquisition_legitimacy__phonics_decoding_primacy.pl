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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics Decoding Primacy in Reading Instruction
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint defines legitimate reading instruction as making the
 *   alphabetic principle explicit through systematic phonics, emphasizing
 *   decoding as primary. It is a specific reading within the broader
 *   'reading_acquisition_legitimacy' kernel, which also includes
 *   'whole_language_meaning_primacy' and 'balanced_literacy_integration'.
 *   This reading advocates for high structure, explicit sequencing, and
 *   decodable texts, with the teacher acting as a direct instructor.
 *   Struggling readers are identified early via decoding assessments. The
 *   constraint's influence has grown over the past two decades, driven by
 *   cognitive science research and policy mandates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.4).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.6).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics Decoding Primacy in Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, 'a1aa92d5-e7b7-47f9-8330-3b2e829b50a3').
narrative_ontology:cs_kernel_codification('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', formalized).
narrative_ontology:cs_authority_grounding('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', expertise).
narrative_ontology:cs_interpretation_layer_present('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3').
narrative_ontology:cs_reading_relation('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', foundational, decoding_is_primary_reading_skill).
narrative_ontology:cs_axiom_status(decoding_is_primary_reading_skill, holdable).
narrative_ontology:cs_axiom_grounding('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', decoding_is_primary_reading_skill, empirically_contingent).
narrative_ontology:cs_axiom('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', foundational, explicit_systematic_phonics_is_optimal_instruction).
narrative_ontology:cs_axiom_status(explicit_systematic_phonics_is_optimal_instruction, holdable).
narrative_ontology:cs_axiom_grounding('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', explicit_systematic_phonics_is_optimal_instruction, empirically_contingent).
narrative_ontology:cs_reference_frame('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', alphabetic_principle_alignment).
narrative_ontology:cs_drift_state('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a1aa92d5-e7b7-47f9-8330-3b2e829b50a3', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, cognitive_science_researchers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_decoding_difficulties).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_whole_language).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, publishers_of_non_decodable_texts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, advocacy_groups_for_reading_disabilities).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, alphabetic_principle).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact legislation and allocate funding that mandates or strongly incentivizes systematic phonics instruction. They respond to research and advocacy, but also face political pressure from various pedagogical camps. Their exit is constrained by electoral cycles and public accountability.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Profit from the increased demand for systematic phonics programs and decodable texts. They actively lobby policy makers and fund research that supports their approach. Their market position is strengthened by this constraint.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, beneficiary,
    organized, biographical, arbitrage, global).

% Their research on reading acquisition, particularly the alphabetic principle and phonological awareness, forms the scientific basis for this constraint. They gain academic influence, funding, and public recognition when their findings are adopted in policy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, cognitive_science_researchers, beneficiary,
    powerful, generational, mobile, global).

% Face pressure to abandon their prior training and adopt new methods, often requiring costly retraining and a shift in professional identity. Their pedagogical philosophy is devalued, and their autonomy is reduced. Exiting the profession is a high cost.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_whole_language, payer,
    moderate, biographical, identity_locked, local).

% Experience reduced demand for their products (e.g., leveled readers, authentic literature without explicit phonics alignment) as schools shift to decodable texts. They must adapt their offerings or lose market share. Their exit is constrained by existing inventory and market position.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, publishers_of_non_decodable_texts, payer,
    organized, biographical, constrained, national).

% Directly benefit from explicit, systematic instruction that addresses their specific learning needs, leading to improved literacy outcomes. Without this instruction, they are often trapped in a cycle of reading failure.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_decoding_difficulties, beneficiary,
    powerless, immediate, trapped, local).

% Actively champion systematic phonics as the most effective approach for students with dyslexia and other reading disabilities. They gain influence and achieve their mission when this pedagogical approach is adopted.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, advocacy_groups_for_reading_disabilities, beneficiary,
    organized, generational, mobile, national).

% Argue for an integrated approach that balances phonics with authentic literature and meaning-making. Their voices are often marginalized in policy debates dominated by the 'science of reading' framework, leading to their exclusion from agenda-setting.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns pedagogical practice across schools and districts with cognitive science research on reading acquisition, ensuring a consistent, evidence-based approach to teaching foundational reading skills.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum development from individual teachers and diverse publishers to cognitive science-informed frameworks and associated curriculum providers. It also transfers resources (funding, professional development) towards systematic phonics programs.
% ABSENT_VOICES: Advocates for whole language and balanced literacy are largely excluded from policy-making and curriculum adoption processes, despite representing significant portions of the educational community. They would argue for a broader definition of reading and more teacher autonomy.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, there would be a rapid diversification of pedagogical approaches, a resurgence of whole language and balanced literacy methods, and a significant shift in curriculum markets. The alignment with cognitive science would weaken, and the 'science of reading' movement would lose its policy leverage.
% FOUNDING_PROBLEM: A perceived crisis in reading proficiency, particularly for struggling learners, and a lack of alignment between pedagogical practice and scientific understanding of how children learn to read.
% FOUNDING_PROBLEM_CORROBORATION: Advocacy groups for reading disabilities, cognitive science researchers, and many policy makers attest that the problem of reading proficiency, especially for vulnerable populations, remains live and that this approach is a necessary solution. Opponents (e.g., balanced literacy advocates) contest the severity of the crisis and the exclusivity of the proposed solution, but generally acknowledge the existence of reading challenges.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).

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
 *   The extractiveness (0.4) is moderate, reflecting the cost of curriculum adoption and teacher retraining, and the exclusion of alternative approaches. Suppression (0.6) is significant due to policy mandates, funding incentives, and professional pressure to conform to 'science of reading' principles. Theater ratio (0.1) is low, as the instruction is genuinely implemented, not merely performed. Accessibility collapse (0.7) is high because alternative pedagogical approaches are actively marginalized. Resistance (0.3) is moderate, coming from educators and researchers who advocate for other methods.
 *
 * PERSPECTIVAL GAP:
 *   Teachers trained in whole language or balanced literacy experience this constraint as highly extractive and suppressive, forcing them to abandon familiar methods and invest in new training. Cognitive science researchers and phonics curriculum publishers, however, experience it as a beneficial coordination mechanism that aligns practice with scientific evidence and creates market opportunities. Students with decoding difficulties are direct beneficiaries, gaining access to instruction tailored to their needs.
 *
 * DIRECTIONALITY LOGIC:
 *   Phonics curriculum publishers and cognitive science researchers are primary beneficiaries (d near 0.0) as their work is validated and commercialized. Teachers trained in whole language and publishers of non-decodable texts are victims (d near 1.0) as their professional capital and market share are diminished. Students with decoding difficulties are beneficiaries (d near 0.0) due to improved outcomes. Policy makers act as agenda-setters, enforcing the shift.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate is actively contested but still live. The 'science of reading' movement provides a continuous justification for its existence, preventing it from becoming a Piton. The ongoing debate with 'whole_language' and 'balanced_literacy' readings ensures its function is regularly re-evaluated, even if the evaluation is contentious.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, independent pedagogical principle, or one reading of the broader ''reading_acquisition_legitimacy'' kernel?',
    'Analysis of policy documents and pedagogical discourse: if the ''phonics_decoding_primacy'' claim is consistently presented as a self-sufficient, foundational truth rather than a specific interpretation within a contested field, it is independent. If it is consistently framed in opposition to other approaches, it is a reading.',
    'If independent, its classification stands alone. If a reading, its classification is understood in relation to sibling readings, and its persistence may be tied to the political economy of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''phonics_decoding_primacy'' reading of the ''reading_acquisition_legitimacy'' kernel.').

omega_variable(
    sibling_reading_impact_whole_language,
    'How would the ''whole_language_meaning_primacy'' sibling reading, if adopted, structurally change this constraint?',
    'Counterfactual policy analysis: if ''whole_language_meaning_primacy'' became dominant, this constraint would be largely foreclosed, leading to a shift away from explicit phonics instruction and decodable texts.',
    'The core tenets of this constraint (explicit phonics, decoding primacy) would be directly contradicted and likely removed from policy and practice, leading to a reclassification of the pedagogical landscape.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_whole_language, conceptual, 'Impact of ''whole_language_meaning_primacy'' on ''phonics_decoding_primacy''.').

omega_variable(
    sibling_reading_impact_balanced_literacy,
    'How would the ''balanced_literacy_integration'' sibling reading, if adopted, structurally change this constraint?',
    'Counterfactual policy analysis: if ''balanced_literacy_integration'' became dominant, this constraint would be integrated but diluted, with explicit phonics becoming one component among others, rather than the primary focus.',
    'The ''primacy'' aspect of this constraint would be diminished, leading to a less intense and less exclusive focus on systematic phonics, potentially shifting its classification towards a more diffuse ''rope'' or ''tangled_rope'' depending on the balance struck.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_balanced_literacy, conceptual, 'Impact of ''balanced_literacy_integration'' on ''phonics_decoding_primacy''.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative pedagogies structural (e.g., policy mandates, funding tied to phonics programs) or internalized (e.g., teachers'' fear of non-compliance, professional identity tied to ''science of reading'')?',
    'Post-policy-change analysis: if alternative pedagogies remain suppressed after mandates are removed, reclassify as partially internalized. If they re-emerge, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as teachers carry the suppression with them even after external barriers are removed. This would amplify the ''snare'' aspects of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for pedagogical approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 5, 0.12).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 10, 0.1).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 15, 0.09).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.05).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, teacher_training_curriculum_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, educational_funding_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel, which also includes 'whole_language_meaning_primacy', 'balanced_literacy_integration', and 'structured_literacy_remediation'. Each reading represents a distinct pedagogical approach with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
