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
 *   human_readable: Balanced Literacy Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'balanced literacy' approach to reading
 *   instruction, which posits that reading acquisition requires both explicit
 *   phonics instruction and meaningful engagement with authentic literature.
 *   It emerged as a compromise in the 'reading wars' between phonics and
 *   whole language proponents. While conceptually balanced, its
 *   implementation often leans heavily towards whole language, leading to
 *   insufficient systematic phonics for many students. This story is one
 *   reading of the 'reading_acquisition_mechanism' kernel, specifically the
 *   'balanced_literacy_reading'.
 *
 * KEY AGENTS:
 *   - educational_publishers: Beneficiary (institutional/mobile)
 *   - teacher_training_institutions: Agenda Setter (institutional/constrained)
 *   - struggling_readers: Payer (powerless/trapped)
 *   - early_career_teachers: Payer (moderate/constrained)
 *   - literacy_researchers: Observer (analytical/analytical)
 *   - parents_of_struggling_readers: Payer (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.45).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.6).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '647967b5-8cf1-4323-a5ee-e8dcf9b14cdd').
narrative_ontology:cs_kernel_codification('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', formalized).
narrative_ontology:cs_authority_grounding('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', lineage).
narrative_ontology:cs_interpretation_layer_present('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd').
narrative_ontology:cs_reading_relation('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', foundational, integrated_instruction_optimizes_acquisition).
narrative_ontology:cs_axiom_status(integrated_instruction_optimizes_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', integrated_instruction_optimizes_acquisition, empirically_contingent).
narrative_ontology:cs_axiom('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', secondary, reading_is_a_natural_process_requiring_scaffolding).
narrative_ontology:cs_axiom_status(reading_is_a_natural_process_requiring_scaffolding, holdable).
narrative_ontology:cs_axiom_grounding('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', reading_is_a_natural_process_requiring_scaffolding, conventional).
narrative_ontology:cs_reference_frame('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', post_reading_wars_synthesis).
narrative_ontology:cs_drift_state('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', contemporary_science_of_reading_movement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('647967b5-8cf1-4323-a5ee-e8dcf9b14cdd', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_institutions).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, early_career_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the demand for diverse instructional materials, including both phonics workbooks and authentic literature sets, which balanced literacy curricula require. They adapt their offerings to fit the prevailing pedagogical consensus.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers, beneficiary,
    institutional, generational, mobile, national).

% Promote balanced literacy as a comprehensive approach, often emphasizing its flexibility and responsiveness to individual student needs. They train new teachers in this methodology, shaping pedagogical practice across the education system.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Often do not receive sufficient systematic phonics instruction under balanced literacy, leading to persistent decoding difficulties. Their academic progress is constrained by the variable implementation fidelity of the approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Are trained in balanced literacy but often struggle to implement its phonics component systematically, especially when faced with large class sizes and diverse student needs. They bear the burden of an often-unclear pedagogical mandate.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, early_career_teachers, payer,
    moderate, biographical, constrained, local).

% Conduct studies on reading acquisition, often highlighting the empirical evidence for systematic phonics. They observe the implementation gaps and outcomes of balanced literacy, providing critical analysis.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, literacy_researchers, observer,
    analytical, generational, analytical, global).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking private tutoring to supplement classroom instruction. They advocate for more explicit and systematic phonics instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, parents_of_struggling_readers, payer,
    organized, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate pedagogical practice by integrating different instructional philosophies (phonics and whole language) into a single, comprehensive framework, reducing ideological conflict among educators.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design influence to institutions promoting integrated approaches, while transferring instructional ambiguity and inconsistent outcomes to teachers and students.
% ABSENT_VOICES: Advocates for pure systematic phonics instruction are often marginalized in mainstream teacher training and curriculum development, arguing that the 'balance' often dilutes effective phonics. Advocates for pure whole language are also excluded, as the phonics component is a concession.
% DISAPPEARANCE_RATIONALE: If the balanced literacy framework vanished, pedagogical practice would likely polarize, with some schools adopting pure phonics and others reverting to whole language, leading to a significant reorganization of curriculum, teacher training, and educational publishing.
% FOUNDING_PROBLEM: The 'reading wars' of the late 20th century created deep divisions in literacy education, with proponents of phonics and whole language unable to agree on a unified approach, leading to inconsistent instructional outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Educational institutions and many teachers attest that the 'reading wars' problem is still live, as the debate continues. Literacy researchers, however, often argue that the scientific evidence for systematic phonics has largely settled the core pedagogical question, making the 'balance' a political rather than an empirical necessity.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).
:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs borne by struggling readers and teachers due to inconsistent implementation, while still acknowledging some coordination benefit. Suppression (0.6) is due to institutional inertia and the difficulty for individual teachers or parents to deviate from established curricula. The high theater ratio (0.55) indicates that the 'phonics' component is often more performative than systematic, masking a de facto whole-language approach in many classrooms. The claimed type is 'tangled_rope' because it genuinely attempts coordination but results in asymmetric extraction due to implementation failures and institutional compromises.
 *
 * PERSPECTIVAL GAP:
 *   Teacher training institutions and educational publishers perceive balanced literacy as a robust, comprehensive framework that resolves prior pedagogical conflicts. Struggling readers and their parents, however, experience it as a system that fails to provide essential foundational skills, leading to significant academic and personal costs. Early career teachers often feel caught between the theoretical mandate and the practical challenges of effective implementation.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational publishers and teacher training institutions are beneficiaries, as the framework supports their existing models and influence. Struggling readers and early career teachers are payers, bearing the costs of its inconsistent application. Parents of struggling readers are also payers, as they must often seek external solutions. Literacy researchers act as observers, analyzing the outcomes without direct benefit or cost from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling balanced literacy as a pure Rope (which would ignore the significant extraction from struggling readers) or a pure Snare (which would ignore its genuine, albeit often unrealized, coordination function in bridging pedagogical divides). The high theater ratio and contested founding problem status suggest a drift towards Mandatrophy, where the 'balance' becomes more about institutional compromise than effective pedagogy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_variability,
    'To what extent does the actual implementation of balanced literacy vary across classrooms and schools, and how does this variability impact student outcomes?',
    'Large-scale observational studies and classroom audits measuring the proportion and systematicity of phonics instruction versus authentic text exposure.',
    'If implementation fidelity is consistently low for phonics, the constraint''s effective extractiveness and theater ratio are higher than measured, pushing it closer to a Snare. If fidelity is high, it moves closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_fidelity_variability, empirical, 'Variability in how balanced literacy is taught, particularly regarding phonics.').

omega_variable(
    institutional_compromise_vs_pedagogical_efficacy,
    'Is balanced literacy primarily an institutional compromise to end the ''reading wars,'' or is it a pedagogically optimal approach supported by robust evidence?',
    'Meta-analysis of longitudinal studies comparing balanced literacy outcomes with pure systematic phonics and pure whole language, controlling for implementation fidelity.',
    'If primarily an institutional compromise, the constraint''s theater ratio is higher, and its coordination function is weaker, making it a more extractive Tangled Rope or even a Piton. If pedagogically optimal, its extractiveness is lower, and its Rope-like qualities are stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_compromise_vs_pedagogical_efficacy, conceptual, 'The underlying motivation and empirical grounding of balanced literacy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of resources, curriculum mandates) or internalized (teachers'' beliefs about ''natural'' reading acquisition, fear of deviating from district policy)?',
    'Teacher surveys and qualitative studies exploring perceived barriers to implementing systematic phonics, combined with analysis of resource allocation and curriculum flexibility.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — teachers carry the suppression with them after exit, making it harder to shift pedagogical practice even with new mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for teachers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(read_tr_t1998, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1998, 0.4).
narrative_ontology:measurement(read_tr_t2006, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2006, 0.5).
narrative_ontology:measurement(read_tr_t2014, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2014, 0.55).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(read_be_t1998, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1998, 0.38).
narrative_ontology:measurement(read_be_t2006, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2006, 0.42).
narrative_ontology:measurement(read_be_t2014, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2014, 0.45).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(read_su_t1998, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(read_su_t2006, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2006, 0.55).
narrative_ontology:measurement(read_su_t2014, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, teacher_professional_development_standards).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_development_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
