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
 *   This constraint describes the 'whole language' approach to reading
 *   instruction, which posits that reading acquisition emerges naturally from
 *   meaningful engagement with connected text, and that explicit phonics
 *   instruction is unnecessary and potentially harmful. It is a reading of
 *   the broader 'literacy_acquisition_kernel' which has been the subject of
 *   intense debate ('the reading wars') for decades. This reading prioritizes
 *   student motivation and holistic engagement over explicit skill
 *   development, leading to benefits for teachers' professional identity but
 *   significant costs for students lacking pre-existing literacy support.
 *
 * KEY AGENTS:
 *   - whole_language_advocates: Agenda setter (institutional/identity_locked) — promotes and defends the approach.
 *   - teachers_professional_identity: Beneficiary (organized/identity_locked) — aligns with pedagogical autonomy.
 *   - students_without_home_literacy_support: Payer (powerless/trapped) — bears the cost of insufficient explicit instruction.
 *   - students_with_dyslexia: Payer (powerless/trapped) — severely disadvantaged by lack of explicit instruction.
 *   - parents_of_struggling_readers: Payer (moderate/constrained) — bears costs, limited influence.
 *   - cognitive_science_researchers: Observer (institutional/analytical) — provides evidence challenging the approach.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.65).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.7).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Pedagogy").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '60b95f92-c3e9-4ed1-b20e-591d9a9a4932').
narrative_ontology:cs_kernel_codification('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', implicit).
narrative_ontology:cs_authority_grounding('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', practice).
narrative_ontology:cs_interpretation_layer_present('60b95f92-c3e9-4ed1-b20e-591d9a9a4932').
narrative_ontology:cs_reading_relation('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', literacy_acquisition_kernel__balanced_literacy_reading, forecloses).
narrative_ontology:cs_reading_relation('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_axiom('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', foundational, reading_is_natural_process).
narrative_ontology:cs_axiom_status(reading_is_natural_process, holdable).
narrative_ontology:cs_axiom_grounding('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', reading_is_natural_process, deontological).
narrative_ontology:cs_axiom('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', foundational, explicit_phonics_harms_motivation).
narrative_ontology:cs_axiom_status(explicit_phonics_harms_motivation, holdable).
narrative_ontology:cs_axiom_grounding('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', explicit_phonics_harms_motivation, empirically_contingent).
narrative_ontology:cs_reference_frame('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', child_centered_emergent_literacy).
narrative_ontology:cs_drift_state('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', contemporary_cognitive_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('60b95f92-c3e9-4ed1-b20e-591d9a9a4932', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teachers_professional_identity).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote the whole language approach, often through teacher training programs, curriculum development, and academic publications. Their professional identity and careers are often tied to this pedagogical framework. They benefit from the autonomy it grants teachers and the focus on 'authentic' reading experiences.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Many teachers find the whole language approach aligns with their professional identity as facilitators of learning and nurturers of motivation, rather than drill instructors. It emphasizes their professional judgment in selecting texts and guiding emergent literacy, rather than following prescriptive phonics programs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teachers_professional_identity, beneficiary,
    organized, biographical, identity_locked, local).

% These students lack the print-rich environments and background knowledge that whole language pedagogy implicitly assumes. Without explicit phonics instruction, they struggle to decode words, fall behind their peers, and may develop negative attitudes towards reading due to persistent difficulty.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support, payer,
    powerless, immediate, trapped, local).

% Students with specific learning disabilities like dyslexia are particularly harmed by the lack of explicit, systematic phonics instruction. Their neurological differences make 'natural' phonics acquisition highly unlikely, leading to severe reading difficulties and academic disadvantage.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia, payer,
    powerless, immediate, trapped, local).

% Often observe their children struggling with reading and seek alternative methods or tutoring. They bear the emotional and financial costs of their children's difficulties, but their ability to influence school curricula is often limited.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Conduct studies on reading acquisition, often finding strong evidence for the necessity of explicit phonics instruction. They observe the outcomes of different pedagogical approaches and provide data that challenges the whole language framework, but their findings may be resisted by entrenched pedagogical communities.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, cognitive_science_researchers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a pedagogical approach that prioritizes meaning-making and engagement with literature, fostering a love of reading by immersing students in connected texts and allowing phonics skills to emerge naturally.
% TRANSFER_FUNCTION: Transfers pedagogical authority and professional validation to teachers who embrace a holistic, less prescriptive approach to literacy, while transferring the burden of decoding acquisition to students' innate abilities and home environments.
% ABSENT_VOICES: Students who fail to acquire reading skills through this method, particularly those from disadvantaged backgrounds or with learning disabilities, are often not heard in the policy debates. Their struggles are sometimes attributed to external factors rather than the pedagogical approach itself.
% DISAPPEARANCE_RATIONALE: If the whole language approach vanished overnight, it would necessitate a fundamental shift in curriculum, teacher training, and classroom practice in many schools. The 'reading wars' would likely intensify as other pedagogical approaches gained dominance, and the professional identity of many educators would be challenged.
% FOUNDING_PROBLEM: The problem of reading instruction becoming overly mechanistic, decontextualized, and demotivating for students, focusing on isolated skills rather than the joy and purpose of reading.
% FOUNDING_PROBLEM_CORROBORATION: Advocates attest that the problem of demotivating, skill-and-drill instruction is still live. Critics (cognitive scientists, parents of struggling readers) acknowledge the historical problem but argue that the whole language solution created new, more severe problems for many students, shifting the nature of the 'live' problem.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).

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
 *   Extractiveness is high (0.65) because the pedagogical approach, while well-intentioned, fails to provide essential skills for a significant portion of students, particularly those from less privileged backgrounds or with learning disabilities, effectively extracting their opportunity for literacy. Suppression (0.7) is high because the professional identity and institutional inertia of whole language advocates actively resist alternative, evidence-based approaches. Theater ratio is low (0.2) as the approach is genuinely implemented, but its effectiveness is contested. Accessibility collapse is moderate (0.4) as alternatives exist (e.g., private tutoring, other schools) but are often costly or inaccessible. Resistance (0.6) is significant from parents and researchers.
 *
 * PERSPECTIVAL GAP:
 *   Whole language advocates perceive this as a beneficial, student-centered approach (low extraction, high coordination). However, students struggling with reading, their parents, and cognitive science researchers experience it as highly extractive and suppressive, as it denies necessary instruction and suppresses evidence-based alternatives. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and teachers' professional identity are beneficiaries (d near 0.0) as the approach validates their pedagogical philosophy and grants autonomy. Students without home literacy support and students with dyslexia are clear targets (d near 1.0) as they bear the direct costs of the pedagogical choice. Parents of struggling readers are also targets, albeit with more agency (d near 0.7). Cognitive science researchers are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to make reading instruction more engaging and meaningful. While this problem is still 'live' (founding_problem_status: live), the 'whole language' solution has arguably created a new problem of widespread reading failure for vulnerable populations. The persistence of the approach, despite mounting evidence for explicit phonics, suggests a degree of mandatrophy where the original coordination function (motivating readers) is now intertwined with extraction from those it fails to serve, sustained by institutional inertia and professional identity. The 'contested' status of the founding problem corroboration highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_efficacy_for_all_students,
    'Does the whole language approach effectively teach reading to all students, particularly those from diverse socioeconomic backgrounds and with learning disabilities?',
    'Longitudinal, large-scale randomized controlled trials comparing whole language outcomes with explicit phonics or structured literacy approaches across diverse student populations.',
    'If evidence shows significant disparities in outcomes, the extractiveness and suppression metrics would be further validated, potentially leading to a reclassification towards Snare for vulnerable student populations. If outcomes are equitable, the coordination function would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pedagogical_efficacy_for_all_students, empirical, 'Empirical evidence on the universal efficacy of whole language pedagogy.').

omega_variable(
    teacher_autonomy_vs_student_outcomes,
    'Is the preservation of teacher autonomy in pedagogical choice a higher priority than ensuring universal reading proficiency through evidence-based methods?',
    'Societal and policy-level deliberation on educational values, potentially leading to legislative mandates for specific instructional methods.',
    'If student outcomes are prioritized, the ''beneficiary'' status of teachers'' professional identity might be re-evaluated, and the constraint could be seen as more extractive. If autonomy is prioritized, the current classification might be seen as more aligned with a ''rope'' for teachers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_student_outcomes, preference, 'The value trade-off between teacher autonomy and universal student outcomes.').

omega_variable(
    identity_lock_vs_evidence,
    'To what extent is the persistence of whole language pedagogy due to genuine belief in its efficacy versus an identity-locked resistance to evidence that challenges professional self-concept?',
    'Qualitative studies of teacher professional development and resistance to change, analysis of institutional responses to research findings, and the career trajectories of advocates.',
    'If identity-lock is the primary mechanism, the ''suppression'' metric is higher due to internalized barriers, and the ''theater_ratio'' might increase as justifications become more performative than functional. This would strengthen the ''tangled_rope'' or even ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_evidence, empirical, 'The role of professional identity in resisting pedagogical change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1970, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(lite_tr_t1980, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(lite_be_t1970, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1970, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel'. Its claims about natural acquisition and the harm of explicit phonics directly influence the perceived legitimacy and adoption of other pedagogical approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
