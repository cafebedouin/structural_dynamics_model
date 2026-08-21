% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__whole_language_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole Language Reading Pedagogy
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'Whole Language' approach to reading
 *   instruction, which posits that reading acquisition emerges naturally from
 *   meaningful engagement with connected text, and that explicit phonics
 *   instruction is unnecessary and potentially harmful to motivation. This is
 *   one reading of the broader 'literacy_acquisition_kernel', which is a
 *   contested domain in educational psychology. The constraint's operation
 *   benefits teachers' professional identity and autonomy but imposes
 *   significant costs on students who lack strong home literacy support, as
 *   they are expected to infer decoding skills implicitly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.8).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.85).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Pedagogy").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '27a67e81-d49d-4015-b0f8-325bfbf45d69').
narrative_ontology:cs_kernel_codification('27a67e81-d49d-4015-b0f8-325bfbf45d69', implicit).
narrative_ontology:cs_authority_grounding('27a67e81-d49d-4015-b0f8-325bfbf45d69', practice).
narrative_ontology:cs_interpretation_layer_present('27a67e81-d49d-4015-b0f8-325bfbf45d69').
narrative_ontology:cs_reading_relation('27a67e81-d49d-4015-b0f8-325bfbf45d69', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('27a67e81-d49d-4015-b0f8-325bfbf45d69', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('27a67e81-d49d-4015-b0f8-325bfbf45d69', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_axiom('27a67e81-d49d-4015-b0f8-325bfbf45d69', foundational, reading_is_natural_process).
narrative_ontology:cs_axiom_status(reading_is_natural_process, holdable).
narrative_ontology:cs_axiom_grounding('27a67e81-d49d-4015-b0f8-325bfbf45d69', reading_is_natural_process, conventional).
narrative_ontology:cs_axiom('27a67e81-d49d-4015-b0f8-325bfbf45d69', foundational, meaning_first_instruction).
narrative_ontology:cs_axiom_status(meaning_first_instruction, holdable).
narrative_ontology:cs_axiom_grounding('27a67e81-d49d-4015-b0f8-325bfbf45d69', meaning_first_instruction, conventional).
narrative_ontology:cs_reference_frame('27a67e81-d49d-4015-b0f8-325bfbf45d69', child_centered_meaning_making).
narrative_ontology:cs_drift_state('27a67e81-d49d-4015-b0f8-325bfbf45d69', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('27a67e81-d49d-4015-b0f8-325bfbf45d69', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_lacking_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, emergent_literacy_theory).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, reading_as_natural_process_theory).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, child_centered_pedagogy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their professional identity and pedagogical methods are affirmed, allowing them autonomy in curriculum design and instruction, which they perceive as beneficial for student engagement and holistic development. Exiting means challenging their professional training and identity.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_teachers, beneficiary,
    moderate, biographical, identity_locked, national).

% Promote and defend the whole language philosophy through teacher training, academic publications, and curriculum development, benefiting from its widespread adoption and the professional status it confers within certain educational circles. They actively enforce its tenets in teacher education.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, constrained, global).

% These students are disproportionately harmed by the lack of explicit phonics instruction, struggling to develop foundational decoding skills. This impacts their overall academic trajectory and future opportunities, as they are trapped within a system that does not meet their learning needs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_lacking_home_literacy_support, payer,
    powerless, immediate, trapped, local).

% Witness their children's difficulties and seek effective reading instruction, but are often limited by school district curriculum choices and may feel disempowered to advocate for alternative methods. Their exit options are limited to changing schools or homeschooling.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Conduct empirical research on reading acquisition, often producing evidence that contradicts whole language tenets, but their findings may be slow to influence pedagogical practice due to institutional inertia and ideological resistance from established educational communities.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, cognitive_scientists_and_researchers, observer,
    analytical, biographical, analytical, global).

% Develop and sell curriculum materials aligned with the whole language philosophy, benefiting from its adoption in school districts and the associated teacher training programs. They have the flexibility to adapt to market demands but profit from the current paradigm.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, curriculum_publishers, beneficiary,
    powerful, biographical, mobile, national).

% Implement and oversee educational policies and curricula, often balancing various pedagogical philosophies and political pressures. They may adopt whole language due to its perceived benefits for teacher morale or philosophical alignment, and enforce its implementation through curriculum mandates.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, policy_makers_and_school_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified pedagogical framework for teaching reading, emphasizing meaning-making and engagement with connected text, which aims to foster a love of reading and teacher autonomy in instructional design.
% TRANSFER_FUNCTION: Transfers pedagogical authority and professional identity to teachers, allowing for flexible, child-centered instruction. Simultaneously, it transfers the burden of implicit skill acquisition to students, particularly those without print-rich home environments, who must infer decoding skills without explicit instruction.
% ABSENT_VOICES: Cognitive scientists emphasizing explicit phonics, parents of struggling readers, and advocates for structured literacy approaches are often marginalized or dismissed in whole language discourse, their concerns framed as overly mechanistic or detrimental to reading motivation.
% DISAPPEARANCE_RATIONALE: If the whole language approach and its associated enforcement mechanisms vanished overnight, schools would immediately shift to explicit phonics or balanced literacy models, teacher training programs would be overhauled, and curriculum materials would be replaced, significantly altering how reading is taught and learned.
% FOUNDING_PROBLEM: The approach was developed as a reaction against overly mechanistic, decontextualized phonics instruction that was perceived to alienate students from the joy and meaning of reading, leading to disengagement and a lack of comprehension.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates claim the problem of decontextualized instruction and student disengagement is still live. However, cognitive scientists and structured literacy advocates attest that the founding problem was misdiagnosed or that the whole language solution created new, more severe problems (e.g., widespread reading difficulties), citing decades of empirical research on reading acquisition and the 'Science of Reading' movement.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because the pedagogical approach, while well-intentioned, fails to provide essential skills for a significant portion of the student population, effectively extracting their potential for literacy. Suppression is very high (0.85) due to the strong ideological stance against explicit phonics, which is often actively discouraged or forbidden in whole language-aligned curricula and teacher training. Theater ratio is low (0.2) because the pedagogical activities (reading aloud, guided reading, writing workshops) are genuinely performed, even if the underlying theory of acquisition is contested. Accessibility collapse (0.75) is high for alternative instructional methods within whole language-dominated systems, and resistance (0.7) is substantial, reflecting the ongoing 'reading wars' and pushback from parents and researchers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language teachers and advocates, the approach is empowering, child-centered, and fosters a love of reading, representing a genuine coordination function. From the perspective of struggling students and their parents, the same structure operates as a barrier to foundational skill acquisition, leading to significant educational and emotional costs. The engine will compute this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language teachers and advocates are beneficiaries (low d) as their professional identity and pedagogical philosophy are affirmed, and they gain autonomy. Curriculum publishers also benefit from selling aligned materials. Students lacking home literacy support and parents of struggling readers are clear targets (high d) as they bear the costs of an instructional method that does not meet their needs, with limited exit options. Policy makers and administrators act as agenda-setters, enforcing the chosen pedagogical approach.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of whole language was to make reading enjoyable and meaningful, counteracting overly mechanistic instruction. However, for many students, particularly those from disadvantaged backgrounds, the approach's failure to provide explicit decoding instruction has led to significant harm, transforming a well-intentioned coordination into an extractive system for this vulnerable group. The founding problem of decontextualized instruction is contested as still live, but the solution's efficacy for all learners is widely disputed by external corroboration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_for_all_learners,
    'Is the whole language approach equally effective for all students, or does its efficacy depend on pre-existing home literacy support and cognitive strengths?',
    'Longitudinal studies comparing reading outcomes across diverse student populations under whole language vs. explicit phonics instruction, controlling for socioeconomic status and cognitive profiles.',
    'If efficacy is highly conditional, the constraint''s extractiveness on vulnerable students is higher than currently measured, and its coordination function is limited to a subset of learners, potentially reclassifying it closer to a Snare for the excluded group.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effectiveness_for_all_learners, empirical, 'Whether whole language provides equitable outcomes across all student demographics.').

omega_variable(
    motivation_vs_skill_tradeoff,
    'Does the whole language emphasis on reading motivation genuinely lead to better long-term literacy, or does it prioritize motivation at the expense of foundational decoding skills, ultimately hindering comprehension?',
    'Studies tracking reading motivation and comprehension outcomes in students taught with whole language vs. explicit phonics, particularly in later grades when texts become more complex.',
    'If foundational skills are demonstrably compromised, the claimed coordination function (fostering love of reading) is undermined by a failure to equip students with the means to read, increasing the effective extraction and potentially shifting the classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(motivation_vs_skill_tradeoff, conceptual, 'The true balance between reading motivation and foundational skill development.').

omega_variable(
    pedagogical_identity_vs_evidence,
    'To what extent is adherence to the whole language approach driven by teachers'' professional identity and philosophical alignment, versus empirical evidence regarding its effectiveness for all learners?',
    'Qualitative studies of teacher decision-making, surveys on pedagogical beliefs, and analysis of resistance to evidence-based reading instruction (e.g., ''Science of Reading'' findings) within educational institutions.',
    'If adherence is primarily identity-driven despite contradictory evidence, the constraint''s persistence is more theatrical and less functional, increasing the theater_ratio and potentially pushing it towards a Piton or a more entrenched Snare, as the coordination story becomes a cover for professional self-preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_identity_vs_evidence, conceptual, 'The role of professional identity versus empirical evidence in sustaining the pedagogical approach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1970, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(lite_tr_t1980, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1980, 0.17).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(lite_be_t1970, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1970, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1990, 0.82).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', focusing on the whole language approach. Its structural properties and metrics are distinct from sibling readings like phonics, balanced literacy, and structured literacy, which offer different pedagogical frameworks and have different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
