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
 *   instruction, which attempts to integrate explicit phonics with exposure
 *   to authentic literature. It is presented as a compromise solution to the
 *   'reading wars' but is increasingly criticized for failing to provide
 *   sufficient systematic phonics instruction, particularly for struggling
 *   readers. This story is one reading of the
 *   'reading_acquisition_legitimacy' kernel, focusing on the integrationist
 *   perspective.
 *
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
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'd2e17e7f-f96a-49ef-a6df-1541f26d6022').
narrative_ontology:cs_kernel_codification('d2e17e7f-f96a-49ef-a6df-1541f26d6022', formalized).
narrative_ontology:cs_authority_grounding('d2e17e7f-f96a-49ef-a6df-1541f26d6022', practice).
narrative_ontology:cs_interpretation_layer_present('d2e17e7f-f96a-49ef-a6df-1541f26d6022').
narrative_ontology:cs_reading_relation('d2e17e7f-f96a-49ef-a6df-1541f26d6022', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('d2e17e7f-f96a-49ef-a6df-1541f26d6022', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('d2e17e7f-f96a-49ef-a6df-1541f26d6022', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('d2e17e7f-f96a-49ef-a6df-1541f26d6022', foundational, reading_is_both_decoding_and_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_both_decoding_and_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('d2e17e7f-f96a-49ef-a6df-1541f26d6022', reading_is_both_decoding_and_meaning_making, conventional).
narrative_ontology:cs_axiom('d2e17e7f-f96a-49ef-a6df-1541f26d6022', secondary, instruction_should_be_eclectic_and_responsive).
narrative_ontology:cs_axiom_status(instruction_should_be_eclectic_and_responsive, holdable).
narrative_ontology:cs_axiom_grounding('d2e17e7f-f96a-49ef-a6df-1541f26d6022', instruction_should_be_eclectic_and_responsive, instrumental).
narrative_ontology:cs_reference_frame('d2e17e7f-f96a-49ef-a6df-1541f26d6022', integrated_pedagogical_synthesis).
narrative_ontology:cs_drift_state('d2e17e7f-f96a-49ef-a6df-1541f26d6022', contemporary_science_of_reading_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2e17e7f-f96a-49ef-a6df-1541f26d6022', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, educational_publishers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement balanced literacy approaches, integrating both explicit phonics and authentic literature. They are expected to differentiate instruction but often lack sufficient training or resources to effectively support all learners, especially those with significant decoding challenges.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, agenda_setter,
    moderate, biographical, constrained, local).

% Benefit from a varied approach that can cater to different learning styles and foster a love of reading through engaging texts. For many, this approach is sufficient for successful reading acquisition.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_students, beneficiary,
    powerless, immediate, trapped, local).

% Often do not receive the intensive, systematic phonics instruction they require, leading to persistent decoding difficulties. They may internalize a belief that they are 'not good at reading,' impacting their academic trajectory and self-esteem.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, biographical, identity_locked, local).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking private tutoring or advocating for more explicit instruction within schools. They face resistance from school systems committed to balanced literacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers, payer,
    organized, generational, constrained, local).

% Profit from selling comprehensive balanced literacy curricula that include a mix of decodable readers, leveled texts, and authentic literature, catering to a broad market without committing exclusively to one pedagogical extreme.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, educational_publishers, beneficiary,
    powerful, generational, arbitrage, national).

% Provide research on the science of reading, often highlighting the critical role of explicit phonics for all learners, especially those at risk. Their findings frequently challenge the efficacy of balanced literacy for certain populations.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse pedagogical approaches to reading instruction, aiming to address both decoding skills and comprehension, and to foster a love of reading, thereby serving a broad range of student needs and teacher preferences.
% TRANSFER_FUNCTION: Transfers pedagogical flexibility and a holistic view of reading to teachers and mainstream students, while transferring the burden of insufficient explicit decoding instruction to struggling readers and their families.
% ABSENT_VOICES: Advocates for systematic, explicit phonics instruction, particularly those representing children with dyslexia and other learning disabilities, are often marginalized in policy discussions that prioritize broader, more 'balanced' approaches. Their voices would highlight the specific failures of this approach for vulnerable learners.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, the landscape of reading instruction would immediately polarize, with schools either adopting purely phonics-based or purely whole-language approaches, leading to significant shifts in curriculum, teacher training, and student outcomes, particularly for struggling readers.
% FOUNDING_PROBLEM: The 'reading wars' of the late 20th century created a need for a compromise approach that acknowledged both the importance of decoding and the value of meaning-making, seeking to end the pedagogical conflict.
% FOUNDING_PROBLEM_CORROBORATION: Educational policymakers and many teachers attest that the 'reading wars' continue, and balanced literacy remains a necessary compromise. However, cognitive scientists and advocates for structured literacy argue that the 'war' has shifted, and the current approach fails to adequately address the scientific consensus on reading acquisition, particularly for struggling learners.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate, as the approach works for many students but imposes significant costs on struggling readers who do not receive adequate support. Suppression (0.30) is present through institutional inertia and the difficulty for parents to challenge established pedagogical norms. Theater ratio (0.20) is low, as the approach does have genuine pedagogical components, but some of its 'balance' may be performative to avoid controversy. The metrics reflect a gradual increase in extractiveness and suppression as the scientific consensus on reading acquisition has solidified, highlighting the costs borne by those for whom the 'balance' is insufficient.
 *
 * PERSPECTIVAL GAP:
 *   Classroom teachers and mainstream students often experience this as a beneficial, flexible approach (closer to a Rope), while struggling readers and their parents experience it as a system that fails them, requiring external intervention (closer to a Snare). Cognitive scientists observe the structural limitations and the gap between pedagogical practice and scientific evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   Classroom teachers and educational publishers benefit from a flexible, marketable approach. Mainstream students benefit from a varied curriculum. Struggling readers and their parents bear the costs of inadequate instruction, often leading to long-term academic and emotional challenges. Cognitive scientists act as analytical observers, identifying the structural issues.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to resolve the 'reading wars' by finding a middle ground. While it achieved a degree of peace, its function for all learners is contested. For struggling readers, the mandate has atrophied, as the approach often fails to deliver effective reading acquisition, yet it persists due to institutional momentum and the political difficulty of adopting a more prescriptive, phonics-heavy approach. The classification as a Rope (claimed) vs. a potential Tangled Rope or Snare (computed for struggling readers) highlights this mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_for_struggling_readers,
    'Is balanced literacy genuinely effective for all learners, particularly those with foundational decoding deficits, or does it systematically underserve them?',
    'Longitudinal studies comparing reading outcomes of struggling readers in balanced literacy programs versus structured literacy programs, controlling for other factors.',
    'If found to systematically underserve struggling readers, the constraint''s extractiveness and suppression would be re-evaluated as significantly higher for this population, potentially shifting its classification towards a Snare for that seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_for_struggling_readers, empirical, 'The true efficacy of balanced literacy for vulnerable student populations.').

omega_variable(
    pedagogical_compromise_vs_scientific_consensus,
    'Is balanced literacy a legitimate pedagogical compromise, or does it represent a political compromise that disregards scientific consensus on reading acquisition?',
    'Analysis of policy documents and expert testimony, comparing the stated rationale for balanced literacy with the findings of cognitive science regarding effective reading instruction.',
    'If primarily a political compromise, the ''coordination'' aspect of the constraint would be seen as largely theatrical, increasing the theater_ratio and shifting the classification towards a Tangled Rope or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_compromise_vs_scientific_consensus, conceptual, 'The underlying nature of balanced literacy as a pedagogical or political construct.').

omega_variable(
    teacher_training_adequacy,
    'Are teachers adequately trained to implement the differentiated instruction required by balanced literacy, especially for students needing intensive phonics?',
    'Surveys of teacher preparedness, analysis of teacher education program curricula, and classroom observation data on instructional practices for struggling readers.',
    'If training is inadequate, the constraint''s effectiveness as a ''Rope'' is undermined, and the burden on struggling readers increases, pushing the classification towards a Snare due to the gap between stated intent and actual implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_training_adequacy, empirical, 'Adequacy of teacher training for effective balanced literacy implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(read_tr_t1998, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(read_tr_t2006, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(read_tr_t2014, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2014, 0.19).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(read_be_t1998, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1998, 0.35).
narrative_ontology:measurement(read_be_t2006, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2006, 0.4).
narrative_ontology:measurement(read_be_t2014, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2014, 0.43).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(read_su_t1998, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1998, 0.25).
narrative_ontology:measurement(read_su_t2006, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2006, 0.28).
narrative_ontology:measurement(read_su_t2014, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2014, 0.29).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, resource_allocation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel, which also includes 'phonics_decoding_primacy', 'whole_language_meaning_primacy', and 'structured_literacy_remediation' as sibling readings. Each represents a distinct approach to reading instruction with different beneficiaries and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
