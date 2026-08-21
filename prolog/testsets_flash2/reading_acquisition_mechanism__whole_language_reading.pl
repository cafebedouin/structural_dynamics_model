% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Mechanism
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'Whole Language' approach to reading
 *   acquisition, which posits that decoding skills emerge implicitly from
 *   meaningful engagement with authentic texts. It is one reading of the
 *   broader 'reading_acquisition_mechanism' kernel, contested by
 *   'phonics_reading' and 'balanced_literacy_reading' approaches. This
 *   reading emphasizes a natural, holistic process, often downplaying the
 *   need for explicit, systematic phonics instruction. The structural delta
 *   for this reading includes low initial instructional cost (no systematic
 *   sequence), high long-term remediation cost for struggling readers,
 *   maximized teacher autonomy, and disproportionate harm to those who do not
 *   implicitly acquire decoding skills.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.65).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.7).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Mechanism").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '72426f27-af73-4de2-83c5-09c2997dadf5').
narrative_ontology:cs_kernel_codification('72426f27-af73-4de2-83c5-09c2997dadf5', distributed).
narrative_ontology:cs_authority_grounding('72426f27-af73-4de2-83c5-09c2997dadf5', practice).
narrative_ontology:cs_interpretation_layer_present('72426f27-af73-4de2-83c5-09c2997dadf5').
narrative_ontology:cs_reading_relation('72426f27-af73-4de2-83c5-09c2997dadf5', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('72426f27-af73-4de2-83c5-09c2997dadf5', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('72426f27-af73-4de2-83c5-09c2997dadf5', foundational, reading_is_natural_language_process).
narrative_ontology:cs_axiom_status(reading_is_natural_language_process, holdable).
narrative_ontology:cs_axiom_grounding('72426f27-af73-4de2-83c5-09c2997dadf5', reading_is_natural_language_process, deontological).
narrative_ontology:cs_axiom('72426f27-af73-4de2-83c5-09c2997dadf5', foundational, decoding_emerges_implicitly).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly, holdable).
narrative_ontology:cs_axiom_grounding('72426f27-af73-4de2-83c5-09c2997dadf5', decoding_emerges_implicitly, empirically_contingent).
narrative_ontology:cs_reference_frame('72426f27-af73-4de2-83c5-09c2997dadf5', holistic_meaning_making_pedagogy).
narrative_ontology:cs_drift_state('72426f27-af73-4de2-83c5-09c2997dadf5', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('72426f27-af73-4de2-83c5-09c2997dadf5', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, curriculum_publishers_wl).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, early_career_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, early_career_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and defend the whole language approach, often holding positions in teacher education, professional organizations, and curriculum development. Their professional identity is deeply tied to this pedagogical philosophy, making exit difficult.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Develop and sell 'authentic literature' based curricula that align with whole language principles. They benefit from the adoption of this approach as it drives sales of their materials, which often require less structured development than phonics programs.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, curriculum_publishers_wl, beneficiary,
    organized, biographical, mobile, national).

% Fail to acquire decoding skills implicitly, leading to significant reading difficulties, academic setbacks, and reduced life opportunities. They are trapped by the pedagogical approach implemented in their schools, with limited access to effective remediation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking expensive private tutoring or advocating for changes in school policy. Their options are constrained by school district policies and lack of pedagogical expertise.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, immediate, constrained, local).

% Are often trained in whole language methods and expected to implement them, even if they observe poor outcomes. They benefit from the perceived autonomy and focus on 'love of reading' but bear the burden of student failure and lack of effective tools for remediation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, early_career_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, early_career_teachers, beneficiary).

% Conduct research on reading acquisition, often finding strong evidence for explicit phonics instruction. They observe the outcomes of different pedagogical approaches and provide evidence-based critiques, but their influence on policy can be slow.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a pedagogical approach that emphasizes meaning-making and engagement with literature, aiming to foster a love of reading and integrate literacy across the curriculum.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design towards holistic, literature-rich instruction, and away from explicit, systematic skill-based teaching. It transfers the burden of skill acquisition from explicit instruction to implicit learning by the student.
% ABSENT_VOICES: Neuroscientists and cognitive psychologists who study the brain's mechanisms for reading acquisition, often finding that explicit phonics instruction is crucial, are often marginalized in educational policy discussions dominated by pedagogical theorists.
% DISAPPEARANCE_RATIONALE: If the whole language approach and its institutional support vanished overnight, teacher training programs would rapidly shift to more explicit, evidence-based methods, curriculum publishers would adapt, and struggling readers would likely receive more effective early intervention, fundamentally altering the landscape of early literacy education.
% FOUNDING_PROBLEM: Early literacy instruction was often rote, decontextualized, and failed to engage children with the joy of reading, leading to disinterest and a lack of comprehension.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates attest the problem is still live, arguing that overemphasis on phonics can kill a love of reading. Cognitive scientists and parents of struggling readers attest that while engagement is important, the core problem of decoding acquisition for many children remains unaddressed by this approach, and that the founding problem of 'disinterest' is often a symptom of decoding failure, not its cause. Independent educational researchers and longitudinal studies corroborate the ineffectiveness for many learners.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the approach fails a significant portion of learners, particularly those with dyslexia or other learning differences, forcing them into expensive and often late remediation. Suppression (0.70) is high due to institutional inertia, teacher training programs, and the suppression of alternative, evidence-based methods in many school districts. The theater ratio (0.40) reflects that while 'love of reading' is a genuine goal, the claim that decoding emerges implicitly is often performative, masking the actual struggle of many students. Accessibility collapse is moderate (0.45) as alternatives exist (e.g., private tutoring, alternative schools) but are often costly or inaccessible. Resistance (0.55) is significant, driven by parents, cognitive scientists, and some educators advocating for evidence-based literacy instruction.
 *
 * PERSPECTIVAL GAP:
 *   Whole language advocates perceive this as a beneficial, child-centered approach that fosters a love of reading, minimizing the 'cost' of explicit instruction. Struggling readers and their parents experience it as a highly extractive and suppressive system that denies them fundamental skills. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and curriculum publishers are beneficiaries, gaining professional influence and market share. Struggling readers and their parents are clear victims, bearing the costs of ineffective instruction. Early career teachers are in a dual role, benefiting from pedagogical autonomy but paying with student failure and professional frustration. Cognitive scientists act as observers, providing critical analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (fostering a love of reading and natural acquisition) has outlived its function for a significant portion of the student population, as scientific evidence increasingly points to the necessity of explicit phonics. The persistence of the approach, despite evidence of its ineffectiveness for many, indicates a form of mandatrophy where institutional inertia and identity-locked advocacy maintain the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_acquisition_efficacy,
    'What percentage of the student population genuinely acquires decoding skills implicitly through whole language methods, without explicit phonics instruction?',
    'Large-scale, longitudinal studies comparing reading outcomes in whole language vs. explicit phonics classrooms, controlling for socioeconomic status and cognitive abilities.',
    'If the percentage is low, the extractiveness and suppression metrics for struggling readers are further validated, strengthening the ''snare'' or ''tangled_rope'' classification. If surprisingly high, it would challenge the current extractiveness assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_acquisition_efficacy, empirical, 'Empirical evidence on the efficacy of implicit decoding acquisition.').

omega_variable(
    teacher_autonomy_vs_student_outcomes,
    'Is the emphasis on teacher autonomy in whole language pedagogy a genuine benefit for all students, or does it primarily benefit teachers by reducing accountability for explicit skill instruction, at the cost of student outcomes?',
    'Qualitative studies on teacher satisfaction and pedagogical freedom correlated with student literacy outcomes across different instructional models, alongside surveys of teacher preparedness for diverse learners.',
    'If teacher autonomy is found to negatively correlate with outcomes for vulnerable students, it would reframe ''teacher autonomy'' as a beneficiary-side extraction, increasing the overall extractiveness score and reinforcing the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_student_outcomes, conceptual, 'The conceptual trade-off between teacher autonomy and student learning outcomes.').

omega_variable(
    identity_lock_strength,
    'How deeply is the professional identity of whole language advocates tied to this pedagogical philosophy, and how does this ''identity lock'' influence resistance to evidence-based reforms?',
    'Sociological studies of educational reform movements, analysis of professional discourse, and interviews with educators who have transitioned between pedagogical approaches.',
    'A strong identity lock suggests higher suppression of alternative views and greater difficulty in implementing evidence-based reforms, reinforcing the persistence of the constraint despite its extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The role of professional identity in maintaining pedagogical approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, curriculum_development_standards).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, teacher_training_accreditation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_mechanism' kernel. Its structural properties and metrics are distinct from sibling readings like 'phonics_reading' and 'balanced_literacy_reading', which are modeled as separate constraints due to differing epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
