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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration in Reading Acquisition
 *   domain: Education Policy / Cognitive Science / Literacy Pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy integration' reading of
 *   the broader 'reading_acquisition_legitimacy' kernel. It posits that
 *   effective reading instruction requires a balance between explicit phonics
 *   (decoding) and authentic literature exposure (meaning-making). While
 *   claimed as a coordination mechanism (rope) to resolve historical
 *   pedagogical conflicts, its implementation often leads to uneven outcomes,
 *   particularly for struggling readers, and requires active enforcement
 *   through curriculum mandates and teacher training. The metrics reflect the
 *   reality of its operation, which can be extractive for certain student
 *   populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.55).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "Education Policy / Cognitive Science / Literacy Pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, '12c45d76-7206-4122-8b1c-3d1c2e8fe4f2').
narrative_ontology:cs_kernel_codification('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', formalized).
narrative_ontology:cs_authority_grounding('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', expertise).
narrative_ontology:cs_interpretation_layer_present('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2').
narrative_ontology:cs_reading_relation('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', foundational, reading_is_dual_process).
narrative_ontology:cs_axiom_status(reading_is_dual_process, holdable).
narrative_ontology:cs_axiom_grounding('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', reading_is_dual_process, empirically_contingent).
narrative_ontology:cs_axiom('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', foundational, instruction_must_be_responsive_to_learner_needs).
narrative_ontology:cs_axiom_status(instruction_must_be_responsive_to_learner_needs, holdable).
narrative_ontology:cs_axiom_grounding('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', instruction_must_be_responsive_to_learner_needs, deontological).
narrative_ontology:cs_reference_frame('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', integrated_pedagogical_synthesis).
narrative_ontology:cs_drift_state('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', contemporary_science_of_reading_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('12c45d76-7206-4122-8b1c-3d1c2e8fe4f2', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_curriculum_developers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, experienced_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, some_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_lacking_training).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set curriculum standards and allocate funding for literacy programs, often promoting balanced literacy as a compromise solution to the 'reading wars'. They mandate teacher training and curriculum adoption.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, education_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Design and sell curriculum materials and professional development programs aligned with balanced literacy principles. They benefit from widespread adoption and the demand for integrated resources.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_curriculum_developers, beneficiary,
    organized, biographical, mobile, global).

% Successfully implement balanced literacy, adapting it to student needs and integrating various components effectively. They benefit from the flexibility and comprehensive nature of the approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, experienced_teachers, beneficiary,
    moderate, biographical, constrained, local).

% May not receive sufficient explicit, systematic phonics instruction within a balanced literacy framework, leading to persistent decoding difficulties and falling behind peers. They bear the cost of an approach not optimally suited to their learning profile.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Struggle to implement balanced literacy effectively due to inadequate professional development, often overemphasizing one component (e.g., authentic literature) at the expense of another (e.g., explicit phonics). They bear the burden of ineffective implementation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_lacking_training, payer,
    moderate, biographical, constrained, local).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking private tutoring or advocating for different instructional methods in schools. They are often excluded from curriculum design decisions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers, payer,
    organized, biographical, constrained, local).

% Argue that balanced literacy does not provide sufficient explicit, systematic phonics instruction, leading to poor outcomes for many students. They are often marginalized in policy discussions that favor a 'balanced' approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_advocates, excluded,
    organized, generational, constrained, national).

% Argue that balanced literacy includes too much explicit phonics and de-emphasizes authentic reading experiences, stifling children's natural development as readers. They are also often marginalized in the current policy discourse.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% Conduct research on reading acquisition and pedagogy, providing evidence that informs (and sometimes critiques) balanced literacy approaches. They observe the system without directly participating in its implementation or extraction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, cognitive_scientists, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate diverse pedagogical insights from both phonics and whole language traditions into a comprehensive approach to reading instruction, aiming to address both decoding and comprehension needs for all learners.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design from single-method approaches to an integrated model. It aims to transfer effective reading skills to students, but in practice, can transfer resources (time, attention) unevenly between explicit phonics and authentic literature, potentially disadvantaging some learners.
% ABSENT_VOICES: Advocates for purely systematic phonics (e.g., Structured Literacy proponents) and purely whole language approaches are often structurally excluded or marginalized in policy discussions that promote 'balance'. They would argue for more focused, less compromised instructional models.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, schools would likely revert to either explicit phonics-heavy or whole language-heavy approaches, or a chaotic mix, leading to significant shifts in curriculum, teacher training, and potentially exacerbating the 'reading wars' once more. The entire literacy education ecosystem would reorganize.
% FOUNDING_PROBLEM: The 'reading wars' of the 20th century, characterized by intense ideological conflict between phonics-first and whole language-first advocates, leading to inconsistent, often ineffective, and politically charged reading instruction.
% FOUNDING_PROBLEM_CORROBORATION: Educational historians and cognitive scientists widely attest to the historical 'reading wars' and the ongoing challenges in literacy acquisition. Independent research and legislative hearings from outside the direct beneficiaries (e.g., parent advocacy groups, academic researchers) corroborate that the founding problem, while mitigated, remains a live concern, particularly for struggling readers.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) is moderate-high because, despite its coordinating intent, balanced literacy often fails to provide sufficient explicit instruction for all learners, particularly those with dyslexia or other learning differences, leading to a 'cost' borne by these students. Suppression (0.55) is moderate as it actively discourages purely phonics-based or purely whole-language curricula in favor of its integrated model, but these alternatives still exist and are advocated for. Theater ratio (0.40) is moderate because while there's genuine effort in implementation, the 'balance' can sometimes be performative, with schools claiming adherence without truly integrating both components effectively. The increasing extractiveness over time reflects growing evidence that its implementation often leaves vulnerable learners behind, despite its continued institutional support.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of policymakers and curriculum developers, balanced literacy is a successful coordination mechanism that synthesizes best practices. From the perspective of struggling readers and their parents, it can be an extractive structure that fails to provide necessary foundational skills. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Education policymakers and curriculum developers are beneficiaries (low d) as they promote and profit from the framework. Experienced teachers also benefit from its flexibility. Struggling readers and teachers lacking adequate training are targets (high d) as they bear the costs of an approach that may not serve them well. Parents of struggling readers are also targets, bearing indirect costs. Phonics and whole language advocates are excluded, their alternative pedagogies suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to resolve the 'reading wars' by integrating different approaches. While the 'reading wars' as a political conflict have evolved, the underlying pedagogical problem of teaching all children to read effectively remains. The 'contested' status of the founding problem, coupled with moderate extractiveness, suggests that while the constraint persists, its effectiveness in fully resolving its mandate is debated, preventing it from being mislabeled as pure extraction or pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_ambiguity,
    'Is the measured extraction primarily due to inherent flaws in the balanced literacy model, or to inconsistent and low-fidelity implementation in schools?',
    'Longitudinal studies comparing student outcomes in schools with high-fidelity balanced literacy implementation versus those with low-fidelity implementation, controlling for teacher training and resources.',
    'If due to implementation, the model itself might be a more effective ''rope'' than currently measured, and interventions should focus on training and resources. If inherent, the model''s extractiveness is a structural feature, requiring pedagogical re-evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_fidelity_ambiguity, empirical, 'Distinguishing between model flaws and implementation failures.').

omega_variable(
    student_outcome_heterogeneity,
    'Does balanced literacy disproportionately extract from specific student populations (e.g., those with dyslexia, low socioeconomic status, or English language learners) compared to others?',
    'Disaggregated student achievement data analyzed by learning profile, socioeconomic status, and language background across different instructional models.',
    'If extraction is highly heterogeneous, the constraint functions as a ''snare'' for vulnerable groups, even if it''s a ''rope'' for others, necessitating targeted interventions or alternative pedagogies for these groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_outcome_heterogeneity, empirical, 'Assessing differential impact on diverse student populations.').

omega_variable(
    pedagogical_consensus_ambiguity,
    'Is the ''balance'' in balanced literacy a genuine pedagogical consensus grounded in cognitive science, or a political compromise designed to end the ''reading wars'' without fully resolving the underlying scientific debates?',
    'Analysis of the scientific literature on reading acquisition, comparing the evidence base for balanced literacy''s integrated approach against the evidence for more explicit or immersive models, and examining policy documents for signs of political negotiation over scientific consensus.',
    'If a political compromise, the constraint''s legitimacy as a ''rope'' is weakened, and its persistence may be more attributable to institutional inertia or political expediency than genuine coordination, potentially shifting its classification towards ''piton'' or ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_consensus_ambiguity, conceptual, 'Nature of the ''balance'' as scientific consensus vs. political compromise.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''balanced_literacy_integration'' reading of the ''reading_acquisition_legitimacy'' kernel. What would change structurally if a sibling reading were adopted?',
    'Comparative analysis of curriculum frameworks and teacher training programs under different dominant pedagogical approaches.',
    'If ''phonics_decoding_primacy'' were adopted, explicit phonics instruction would increase, authentic literature exposure might decrease, and struggling readers might benefit more from foundational skills. If ''whole_language_meaning_primacy'' were adopted, the reverse would occur, potentially benefiting fluent readers but disadvantaging those needing explicit decoding. If ''structured_literacy_remediation'' were adopted, instruction would become more explicit, systematic, and diagnostic for all, with a stronger focus on early intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Structural changes under alternative readings of the reading acquisition kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(read_tr_t1996, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1996, 0.3).
narrative_ontology:measurement(read_tr_t2002, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2002, 0.35).
narrative_ontology:measurement(read_tr_t2008, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(read_tr_t2014, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2014, 0.39).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(read_be_t1996, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1996, 0.55).
narrative_ontology:measurement(read_be_t2002, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2002, 0.6).
narrative_ontology:measurement(read_be_t2008, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2008, 0.63).
narrative_ontology:measurement(read_be_t2014, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(read_su_t1996, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1996, 0.5).
narrative_ontology:measurement(read_su_t2002, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2002, 0.53).
narrative_ontology:measurement(read_su_t2008, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2008, 0.54).
narrative_ontology:measurement(read_su_t2014, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_training_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_development_funding).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel, alongside 'phonics_decoding_primacy', 'whole_language_meaning_primacy', and 'structured_literacy_remediation'. Each represents a distinct pedagogical approach to reading instruction, with different structural implications and beneficiary/victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
