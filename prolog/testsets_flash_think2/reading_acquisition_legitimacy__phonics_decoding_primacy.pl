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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics-First Decoding Primacy in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of reading
 *   acquisition legitimacy, which asserts that reading is fundamentally
 *   decoding and that legitimate instruction must explicitly teach the
 *   alphabetic principle through systematic phonics. This position has gained
 *   significant traction, particularly through the 'Science of Reading'
 *   movement, leading to widespread policy adoption and curriculum mandates.
 *   It stands in contrast to approaches that emphasize meaning-making or a
 *   more balanced integration of skills.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.75).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics-First Decoding Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '9c64379b-4a53-4342-8561-6883db3be797').
narrative_ontology:cs_kernel_codification('9c64379b-4a53-4342-8561-6883db3be797', formalized).
narrative_ontology:cs_authority_grounding('9c64379b-4a53-4342-8561-6883db3be797', expertise).
narrative_ontology:cs_interpretation_layer_present('9c64379b-4a53-4342-8561-6883db3be797').
narrative_ontology:cs_reading_relation('9c64379b-4a53-4342-8561-6883db3be797', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('9c64379b-4a53-4342-8561-6883db3be797', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('9c64379b-4a53-4342-8561-6883db3be797', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('9c64379b-4a53-4342-8561-6883db3be797', foundational, alphabetic_principle_is_primary).
narrative_ontology:cs_axiom_status(alphabetic_principle_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('9c64379b-4a53-4342-8561-6883db3be797', alphabetic_principle_is_primary, empirically_contingent).
narrative_ontology:cs_axiom('9c64379b-4a53-4342-8561-6883db3be797', foundational, explicit_systematic_instruction_is_optimal).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('9c64379b-4a53-4342-8561-6883db3be797', explicit_systematic_instruction_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('9c64379b-4a53-4342-8561-6883db3be797', science_of_reading_consensus).
narrative_ontology:cs_drift_state('9c64379b-4a53-4342-8561-6883db3be797', contemporary_policy_adoption, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9c64379b-4a53-4342-8561-6883db3be797', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, students_acquiring_decoding_skills).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, explicit_instruction_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, students_struggling_with_phonics).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_implementing_phonics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and market systematic phonics curricula and materials. They directly benefit from policy mandates that prioritize explicit phonics instruction, leading to widespread adoption of their products.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Researchers, educators, and parent groups who champion systematic phonics based on cognitive science. They benefit from the validation and widespread adoption of their preferred pedagogical approach, seeing it as a victory for evidence-based practice.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, explicit_instruction_advocates, beneficiary,
    organized, generational, mobile, national).

% Students for whom explicit, systematic phonics instruction is an effective and efficient pathway to decoding proficiency. They benefit from clear, structured instruction that enables them to read words.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_acquiring_decoding_skills, beneficiary,
    powerless, immediate, constrained, local).

% Students who do not thrive under a rigid, phonics-first approach, potentially due to learning differences or a need for broader literacy experiences. They bear the cost of a one-size-fits-all method that may not meet their individual learning needs, leading to frustration and disengagement.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_struggling_with_phonics, payer,
    powerless, immediate, trapped, local).

% Educators who must implement mandated phonics curricula, often requiring retraining and a shift away from prior pedagogical beliefs. They bear the cost of adapting their practice and potentially sacrificing other instructional methods they believe are valuable.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_implementing_phonics, payer,
    moderate, biographical, constrained, local).

% Educators and theorists who prioritize meaning-making and immersion in authentic literature, believing decoding skills emerge naturally. They are largely excluded from policy-making and curriculum development under the phonics-first paradigm, seeing their approach discredited.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% Teachers who previously used a balanced approach, integrating phonics with other literacy strategies. They are pressured to increase explicit phonics instruction, often feeling their professional autonomy is diminished and their holistic view of literacy is undervalued.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_teachers, payer,
    moderate, biographical, constrained, local).

% Academics who study reading acquisition from a scientific perspective. While many support the alphabetic principle, they observe the policy debates and implementation challenges, often noting nuances lost in policy translation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, cognitive_science_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes reading instruction across schools and districts to ensure all students are taught a foundational decoding skill, providing a common, explicit framework for early literacy development and assessment.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design from diverse, often teacher-led, approaches to a specific, explicit phonics methodology. This moves resources (funding, training, materials) towards phonics-aligned products and away from other literacy programs.
% ABSENT_VOICES: Whole language advocates and those who prioritize meaning-making, authentic literature, and a broader view of literacy over decoding in early instruction are often marginalized or excluded from policy discussions. They would argue that this phonics-first approach, while effective for some, can stifle a love of reading, neglect comprehension, and fail to address the diverse needs of all learners.
% DISAPPEARANCE_RATIONALE: If the primacy of phonics decoding vanished overnight, educational policy and practice would immediately diversify. There would be a resurgence of whole language and balanced literacy approaches, a significant shift in curriculum development, and a re-evaluation of teacher training, fundamentally reorganizing how reading is taught and assessed.
% FOUNDING_PROBLEM: High rates of illiteracy and reading difficulties, particularly among disadvantaged students, attributed to inconsistent or ineffective reading instruction that lacked explicit foundational skills in decoding, leading to a call for evidence-based methods.
% FOUNDING_PROBLEM_CORROBORATION: Educational researchers and policymakers aligned with the 'Science of Reading' movement corroborate the ongoing problem of reading failure and the efficacy of phonics-first approaches. While the existence of reading difficulties is widely acknowledged, opponents contest the *sole cause* of the problem and the *universal applicability* of the phonics-first solution, arguing for broader systemic factors and diverse pedagogical needs.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because this approach, while effective for many, imposes a specific, rigorous method that can limit pedagogical diversity and potentially disengage learners for whom it is not the optimal pathway. Suppression (0.75) is also high, driven by policy mandates, curriculum requirements, and the active discrediting of alternative methods (e.g., whole language, balanced literacy) in public discourse and teacher training. The theater ratio (0.15) is low because the constraint is highly functional and actively implemented, not merely performative. Resistance (0.70) is high due to ongoing 'reading wars' debates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of phonics advocates, this constraint is a 'Rope' or even a 'Mountain' – a scientifically validated, efficient solution to a critical problem, benefiting all. From the perspective of those whose methods are suppressed or students who struggle, it operates as a 'Snare' or 'Tangled Rope,' imposing a costly, restrictive, and potentially harmful uniformity. The engine's classification as 'Tangled Rope' reflects this dual nature: a genuine coordination function (teaching reading) coupled with significant asymmetric extraction and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Phonics curriculum publishers and explicit instruction advocates are clear beneficiaries, gaining market share and validation. Students who successfully acquire decoding skills also benefit. Conversely, students who struggle with this specific method, and teachers who prefer or are trained in alternative approaches (like balanced literacy), bear the costs of adaptation or disengagement. Whole language advocates are largely excluded, their methods suppressed. Cognitive science researchers act as observers, providing evidence that is often selectively interpreted by advocates.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_truth_vs_policy_choice,
    'Is the primacy of phonics decoding a universal pedagogical truth derived from cognitive science, or a policy choice influenced by specific advocacy groups and market interests?',
    'Longitudinal studies comparing diverse instructional methods across varied student populations, controlling for socioeconomic factors and teacher training quality, to determine if phonics-first universally outperforms other approaches or if optimal instruction is context-dependent.',
    'If a universal truth, the constraint''s extractiveness and suppression might be re-evaluated as necessary costs of optimal coordination. If a policy choice, the extractive and suppressive elements would be seen as rent-seeking or ideological imposition, strengthening its ''Snare'' or ''Tangled Rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_truth_vs_policy_choice, conceptual, 'Ambiguity between scientific consensus and policy advocacy in literacy instruction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative reading instruction methods primarily structural (e.g., policy mandates, curriculum requirements) or internalized (e.g., teachers feeling professional pressure to conform, fear of criticism)?',
    'Post-mandate policy shifts: if alternative methods resurface quickly and effectively when mandates are relaxed, suppression is largely structural. If teachers continue to avoid alternatives due to ingrained beliefs or fear, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as teachers carry the suppression with them even after external barriers are removed. This would amplify the ''Snare'' aspects of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for pedagogical methods.').

omega_variable(
    universal_benefit_vs_differential_harm,
    'Does the universal application of phonics-first instruction genuinely benefit all learners, or does it differentially harm some learners by neglecting other crucial aspects of literacy development (e.g., comprehension, vocabulary, love of reading)?',
    'Comprehensive assessments that track not only decoding skills but also reading comprehension, motivation, and engagement over time for diverse student cohorts under different instructional models. Qualitative data on student and teacher experiences.',
    'If differential harm is significant, the ''victim'' set would expand, and the constraint''s extractiveness would be seen as more severe, as it extracts not just resources but also potential for holistic literacy development from certain students. This would push the classification closer to ''Snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_benefit_vs_differential_harm, empirical, 'Whether the benefits of phonics-first are universal or come at a cost to some learners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(read_tr_t1995, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(read_be_t1995, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(read_su_t1995, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, curriculum_development_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, teacher_training_certification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
