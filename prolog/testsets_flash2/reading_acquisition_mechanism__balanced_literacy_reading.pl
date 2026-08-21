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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Approach to Reading Acquisition
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy' approach to reading
 *   instruction, which posits that reading acquisition requires both explicit
 *   phonics instruction and authentic literature exposure in an integrated
 *   practice. This is one reading of the broader 'reading acquisition
 *   mechanism' kernel, which is highly contested in educational psychology.
 *   This reading attempts to bridge the 'reading wars' but often suffers from
 *   variable implementation fidelity, frequently collapsing to a de facto
 *   whole-language approach due to insufficient systematic phonics
 *   instruction in practice. The constraint is claimed as a Rope
 *   (coordination) but its metrics reflect a Tangled Rope due to the
 *   asymmetric costs borne by struggling readers and early-career teachers.
 *
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
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Approach to Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, 'b0dbc54c-4f1e-495c-8592-3a3e93874b6d').
narrative_ontology:cs_kernel_codification('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', formalized).
narrative_ontology:cs_authority_grounding('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', lineage).
narrative_ontology:cs_interpretation_layer_present('b0dbc54c-4f1e-495c-8592-3a3e93874b6d').
narrative_ontology:cs_reading_relation('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', foundational, integrated_skills_holistic_development).
narrative_ontology:cs_axiom_status(integrated_skills_holistic_development, holdable).
narrative_ontology:cs_axiom_grounding('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', integrated_skills_holistic_development, conventional).
narrative_ontology:cs_axiom('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', secondary, reading_is_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', reading_is_meaning_making, conventional).
narrative_ontology:cs_reference_frame('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', post_reading_wars_synthesis).
narrative_ontology:cs_drift_state('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', contemporary_classroom_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0dbc54c-4f1e-495c-8592-3a3e93874b6d', '').
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

% Benefit from the demand for diverse instructional materials (phonics workbooks, leveled readers, authentic texts) that the balanced literacy approach requires. They adapt their offerings to fit the prevailing pedagogical consensus.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers, beneficiary,
    institutional, generational, mobile, national).

% Promote and teach balanced literacy as the comprehensive, evidence-informed approach, integrating it into curriculum and certification. They benefit from its perceived inclusivity and ability to bridge historical pedagogical divides.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Often do not receive sufficiently systematic phonics instruction, leading to persistent decoding difficulties. They bear the cost of an approach that, in practice, frequently under-emphasizes the explicit skills they need most, leading to academic and social struggles.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, identity_locked, local).

% Are trained in balanced literacy but often struggle to implement its phonics component systematically due to inadequate preparation, time constraints, and pressure to cover broad curriculum. They bear the burden of an approach that is difficult to execute effectively in diverse classrooms.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, early_career_teachers, payer,
    moderate, biographical, constrained, local).

% Analyze the efficacy of balanced literacy, often finding discrepancies between its theoretical claims and practical outcomes, particularly regarding phonics instruction. Their findings frequently challenge the status quo but face institutional inertia.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, literacy_researchers, observer,
    analytical, generational, analytical, global).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking private tutoring to supplement classroom instruction. They advocate for more explicit and systematic phonics but face resistance from entrenched pedagogical practices.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, parents_of_struggling_readers, payer,
    organized, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate diverse pedagogical theories (phonics, whole language) into a single, comprehensive framework for reading instruction, providing a common language and set of practices for educators and curriculum developers.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum development resources to institutions and publishers aligned with the balanced literacy framework, while transferring the burden of inconsistent implementation and insufficient skill development to students and teachers.
% ABSENT_VOICES: Advocates for explicit, systematic phonics (often cognitive scientists and parents of dyslexic children) are often marginalized in mainstream educational discourse, their concerns framed as 'narrow' or 'drill-and-kill' despite strong empirical support for foundational skills.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished overnight, there would be a significant pedagogical vacuum. Schools would likely revert to either pure phonics or pure whole language approaches, or a new, more evidence-aligned synthesis would rapidly emerge, reorganizing curriculum, teacher training, and publishing.
% FOUNDING_PROBLEM: To resolve the 'reading wars' between phonics and whole language advocates by integrating elements of both into a 'best of both worlds' approach, aiming for comprehensive literacy development.
% FOUNDING_PROBLEM_CORROBORATION: Educational institutions and many teachers attest the problem is live, citing the need for a holistic approach. Cognitive scientists and advocates for explicit phonics attest the founding problem (the 'reading wars') was largely a false dichotomy, and that balanced literacy often fails to adequately address the core problem of decoding for many students; independent research corroborates the implementation fidelity issues.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is moderate because while some students benefit, many struggling readers pay a high cost due to inadequate phonics. Suppression (0.6) is significant as alternative, more explicit phonics-focused approaches are often marginalized or actively resisted within educational institutions. Theater ratio (0.55) is high because the 'phonics' component is often performative or unsystematic, masking a de facto whole-language practice. Accessibility collapse (0.4) is moderate as alternative pedagogical approaches exist but are constrained by institutional consensus. Resistance (0.3) is present from parents and some researchers but is not strong enough to overturn the dominant paradigm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of teacher training institutions, balanced literacy is a robust, comprehensive approach that coordinates diverse pedagogical insights. From the perspective of struggling readers and their parents, it is an extractive system that fails to provide essential foundational skills, leading to significant educational disadvantage. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational publishers and teacher training institutions are beneficiaries, as balanced literacy supports their existing structures and material production. Struggling readers and early-career teachers are payers, bearing the costs of an often-ineffective or difficult-to-implement approach. Parents of struggling readers are also payers, as they often seek external support. Literacy researchers act as observers, analyzing the constraint's effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to resolve the 'reading wars' and provide a holistic approach. However, its implementation often drifts, leading to a theatrical maintenance of the phonics component while the actual practice leans towards whole language. This prevents mislabeling it as pure coordination (Rope) when it has clear extractive elements and a high theater ratio, indicating a drift towards a Piton or Snare for certain seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_variability,
    'To what extent is the ''phonics'' component of balanced literacy actually implemented systematically and explicitly in classrooms, versus being performative or implicit?',
    'Large-scale observational studies of classroom instruction, curriculum analysis, and teacher interviews focusing on the depth and consistency of phonics delivery.',
    'If implementation fidelity is consistently low for phonics, the constraint''s effective extractiveness and theater ratio are higher, pushing it closer to a Snare or Piton for struggling readers. If fidelity is high, it moves closer to a genuine Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_fidelity_variability, empirical, 'Assesses the gap between the stated balanced literacy approach and its actual classroom execution, particularly for phonics.').

omega_variable(
    pedagogical_consensus_vs_evidence,
    'Is the persistence of balanced literacy driven by a genuine pedagogical consensus reflecting the best available evidence, or by institutional inertia and the difficulty of shifting entrenched practices?',
    'Analysis of educational policy changes in response to new cognitive science research on reading, and the speed of curriculum adoption cycles. Examine the influence of professional organizations versus independent research bodies.',
    'If institutional inertia is the primary driver, the constraint''s suppression and theater ratio are higher, indicating a Tangled Rope or Snare. If evidence-driven consensus, it moves closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_consensus_vs_evidence, conceptual, 'Examines the drivers of balanced literacy''s persistence: evidence-based consensus or institutional resistance to change.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative reading approaches structural (e.g., curriculum mandates, certification requirements) or internalized (e.g., teachers'' beliefs about ''best practice'' despite conflicting evidence)?',
    'Qualitative studies of teacher decision-making and professional development content, alongside analysis of state-level curriculum frameworks and textbook adoption processes. If suppression persists after structural barriers are removed, it''s partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, as teachers carry the suppression with them. This makes it harder to shift pedagogical practice even with policy changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative reading pedagogies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 25, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 25, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, teacher_certification_standards).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_mechanism' kernel, alongside 'phonics_reading' and 'whole_language_reading'. Each represents a distinct pedagogical approach with different structural properties and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
