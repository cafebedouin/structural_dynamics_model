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
 *   human_readable: Phonics-First Reading Acquisition (Literacy Kernel Reading)
 *   domain: educational_psychology/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of literacy
 *   acquisition, asserting that explicit, systematic instruction in
 *   phoneme-grapheme correspondence is a prerequisite for successful reading
 *   comprehension. It is one reading of the broader
 *   'literacy_acquisition_kernel'. The constraint is claimed as a Tangled
 *   Rope because it genuinely coordinates foundational skill development
 *   (benefiting struggling readers) but also extracts from teacher autonomy
 *   and potentially from advanced readers through rigid adherence to scripted
 *   programs. The metrics reflect the increasing institutionalization and
 *   enforcement of this approach over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.65).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.7).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Reading Acquisition (Literacy Kernel Reading)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '2ad7eb8c-0693-481a-bff5-4281d80518ff').
narrative_ontology:cs_kernel_codification('2ad7eb8c-0693-481a-bff5-4281d80518ff', formalized).
narrative_ontology:cs_authority_grounding('2ad7eb8c-0693-481a-bff5-4281d80518ff', expertise).
narrative_ontology:cs_interpretation_layer_present('2ad7eb8c-0693-481a-bff5-4281d80518ff').
narrative_ontology:cs_reading_relation('2ad7eb8c-0693-481a-bff5-4281d80518ff', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('2ad7eb8c-0693-481a-bff5-4281d80518ff', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('2ad7eb8c-0693-481a-bff5-4281d80518ff', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('2ad7eb8c-0693-481a-bff5-4281d80518ff', foundational, decoding_is_primary_bottleneck).
narrative_ontology:cs_axiom_status(decoding_is_primary_bottleneck, holdable).
narrative_ontology:cs_axiom_grounding('2ad7eb8c-0693-481a-bff5-4281d80518ff', decoding_is_primary_bottleneck, empirically_contingent).
narrative_ontology:cs_axiom('2ad7eb8c-0693-481a-bff5-4281d80518ff', foundational, explicit_instruction_is_most_efficient).
narrative_ontology:cs_axiom_status(explicit_instruction_is_most_efficient, holdable).
narrative_ontology:cs_axiom_grounding('2ad7eb8c-0693-481a-bff5-4281d80518ff', explicit_instruction_is_most_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('2ad7eb8c-0693-481a-bff5-4281d80518ff', scientific_consensus_on_decoding).
narrative_ontology:cs_drift_state('2ad7eb8c-0693-481a-bff5-4281d80518ff', contemporary_education_policy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2ad7eb8c-0693-481a-bff5-4281d80518ff', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_phonics_programs).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, students_with_strong_phonological_awareness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These students benefit significantly from explicit, systematic phonics instruction, as it provides the foundational decoding skills they need to access text. Without it, they often struggle to read and fall behind their peers.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, immediate, trapped, local).

% These publishers profit from the widespread adoption of phonics-first curricula, which often involve scripted lessons and specific materials. They actively advocate for policies that mandate systematic phonics instruction.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_phonics_programs, beneficiary,
    organized, generational, arbitrage, national).

% Teachers are often required to follow highly scripted phonics programs, limiting their autonomy to adapt instruction to individual student needs or integrate other literacy approaches. This can lead to professional dissatisfaction and a feeling of de-skilling.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment, payer,
    moderate, biographical, constrained, local).

% These students may find highly repetitive phonics instruction tedious and demotivating, as they often acquire decoding skills more easily and are ready for more complex text engagement. The constraint can slow their progress and reduce their enjoyment of reading.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_strong_phonological_awareness, payer,
    powerless, immediate, trapped, local).

% Administrators implement and enforce curriculum mandates, often driven by state or district policies that favor phonics-first approaches. They balance pressure from policymakers, parents, and teacher unions.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, educational_administrators, agenda_setter,
    institutional, biographical, constrained, regional).

% Parents whose children struggle with reading often advocate for explicit phonics instruction, seeing it as a clear, actionable solution. They benefit from the perceived effectiveness and structured nature of these programs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, local).

% Researchers in cognitive science study the mechanisms of reading acquisition, providing empirical evidence that often supports the importance of phonological awareness and decoding. Their findings influence pedagogical debates and policy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates literacy instruction around a scientifically supported sequence of skill development, ensuring that all students, especially those at risk, receive explicit decoding instruction before being overwhelmed by complex texts.
% TRANSFER_FUNCTION: Transfers instructional time and pedagogical authority from holistic, meaning-focused approaches to explicit, systematic phonics instruction, from teachers' discretionary judgment to prescribed curricula, and from students with strong phonological awareness to those with weak awareness.
% ABSENT_VOICES: Advocates for 'whole language' or 'balanced literacy' approaches, who would argue for more integrated, meaning-rich instruction from the outset, are often marginalized in policy debates that emphasize phonics-first mandates. Their perspectives are often dismissed as 'unscientific' or 'outdated'.
% DISAPPEARANCE_RATIONALE: If the phonics-first mandate disappeared overnight, many schools would likely revert to more eclectic or 'balanced' approaches, potentially leaving struggling readers without the systematic support they need. Curriculum markets would diversify, and teacher autonomy would increase, but with potentially inconsistent outcomes for students.
% FOUNDING_PROBLEM: A significant number of students, particularly those from disadvantaged backgrounds or with specific learning differences, were failing to acquire basic reading skills under less explicit instructional methods, leading to widespread illiteracy and educational inequity.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science research consistently corroborates the importance of phonological awareness and explicit phonics for early reading acquisition, especially for at-risk learners. Educational outcomes data from various regions also show improved decoding skills where systematic phonics is implemented. This corroboration comes from independent researchers and educational data analysts, not just curriculum publishers.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.65) due to the imposition of scripted curricula and the de-emphasis of teacher professional judgment, as well as the potential disengagement of students who don't require such intensive phonics. Suppression (0.70) is high because alternative pedagogical approaches are actively suppressed or marginalized in policy and curriculum mandates. Theater ratio is low (0.10) as the instruction is generally direct and functional, not performative. The temporal measurements show a gradual increase in extractiveness and suppression as phonics-first mandates have gained traction and become more entrenched in educational systems.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of struggling readers and their parents, this constraint is a necessary rope, providing essential scaffolding. From the perspective of many teachers and advanced readers, it can feel like a snare, limiting pedagogical freedom and stifling engagement. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with weak phonological awareness and curriculum publishers are clear beneficiaries. Teachers' professional judgment and students with strong phonological awareness are victims, bearing the costs of reduced autonomy or suboptimal learning experiences. Educational administrators act as agenda-setters, mediating between policy pressures and classroom realities. Parents of struggling readers are beneficiaries, as they see a clear, structured approach to their children's difficulties. Cognitive scientists act as observers, providing empirical grounding for the debate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    teacher_autonomy_vs_student_outcomes,
    'To what extent does strict adherence to phonics-first mandates genuinely improve overall student reading comprehension, versus merely improving decoding at the expense of other literacy skills or teacher professional judgment?',
    'Longitudinal studies comparing student outcomes (decoding, comprehension, reading motivation) in classrooms with high-fidelity phonics implementation versus those with more balanced or teacher-adaptive approaches, controlling for student demographics and teacher experience.',
    'If overall comprehension and motivation are not significantly improved, or if they decline for some students, the extractiveness on teacher autonomy and certain student groups would be re-evaluated as less justified by coordination benefits, potentially shifting the classification towards a Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_student_outcomes, empirical, 'Trade-off between prescriptive phonics instruction and broader literacy development.').

omega_variable(
    curriculum_market_influence,
    'Is the persistence and increasing enforcement of phonics-first mandates primarily driven by robust scientific consensus, or by the lobbying and market power of curriculum publishers who benefit from these mandates?',
    'Analysis of legislative and policy-making processes, including campaign contributions, lobbying expenditures, and the composition of curriculum adoption committees, alongside independent reviews of the scientific literature.',
    'Strong evidence of publisher-driven influence would increase the perceived extractiveness and suppression associated with the ''curriculum_publishers_of_phonics_programs'' seat, potentially reclassifying the constraint as a Snare for the ''teachers_professional_judgment'' seat, as the coordination story would be revealed as cover for rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curriculum_market_influence, empirical, 'Influence of commercial interests on pedagogical policy.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the ''literacy_acquisition_kernel'' best framed as a scientific question about cognitive processes, or as a policy question about educational equity and resource allocation?',
    'Analysis of the dominant discourse in policy documents, research funding priorities, and public debate. If the debate is primarily about ''what works'' for all students, it''s scientific; if it''s about ''who gets to read'' and ''who pays for effective instruction'', it''s policy.',
    'Framing as a scientific question tends to legitimize phonics-first approaches by appealing to ''evidence'', potentially masking the extractive elements on teacher autonomy. Framing as a policy question would highlight the distributional consequences and power dynamics, making the extractive elements more salient and contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Scientific vs. policy framing of the literacy acquisition debate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__phonics_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__phonics_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel'. Its claims about explicit phonics instruction influence and are influenced by other pedagogical approaches to reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
