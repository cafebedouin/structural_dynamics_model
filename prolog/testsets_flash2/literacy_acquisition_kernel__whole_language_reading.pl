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
 *   This constraint represents the 'whole language' approach to literacy
 *   pedagogy, which posits that reading acquisition emerges naturally from
 *   meaningful engagement with connected text, and that explicit phonics
 *   instruction is unnecessary or even harmful. It is one reading of the
 *   broader 'literacy_acquisition_kernel' which has been subject to intense
 *   debate ('the reading wars'). This reading emphasizes teacher autonomy and
 *   student motivation, but often at the cost of foundational decoding skills
 *   for vulnerable student populations.
 *
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
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Pedagogy").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, 'fb94e9ee-21c4-420d-abcd-62887535b94c').
narrative_ontology:cs_kernel_codification('fb94e9ee-21c4-420d-abcd-62887535b94c', distributed).
narrative_ontology:cs_authority_grounding('fb94e9ee-21c4-420d-abcd-62887535b94c', practice).
narrative_ontology:cs_interpretation_layer_present('fb94e9ee-21c4-420d-abcd-62887535b94c').
narrative_ontology:cs_reading_relation('fb94e9ee-21c4-420d-abcd-62887535b94c', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb94e9ee-21c4-420d-abcd-62887535b94c', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb94e9ee-21c4-420d-abcd-62887535b94c', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('fb94e9ee-21c4-420d-abcd-62887535b94c', foundational, reading_is_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('fb94e9ee-21c4-420d-abcd-62887535b94c', reading_is_meaning_making, deontological).
narrative_ontology:cs_axiom('fb94e9ee-21c4-420d-abcd-62887535b94c', foundational, phonics_emerges_naturally).
narrative_ontology:cs_axiom_status(phonics_emerges_naturally, holdable).
narrative_ontology:cs_axiom_grounding('fb94e9ee-21c4-420d-abcd-62887535b94c', phonics_emerges_naturally, empirically_contingent).
narrative_ontology:cs_reference_frame('fb94e9ee-21c4-420d-abcd-62887535b94c', holistic_meaning_centered_pedagogy).
narrative_ontology:cs_drift_state('fb94e9ee-21c4-420d-abcd-62887535b94c', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fb94e9ee-21c4-420d-abcd-62887535b94c', '').
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

% Promote and defend the whole language approach, often through academic publications, teacher training programs, and curriculum development. They benefit from the professional identity and academic standing associated with this pedagogical tradition.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, constrained, national).

% Many teachers find the whole language approach aligns with their professional identity as facilitators of meaning-making, rather than technical skill instructors. Shifting to explicit phonics can feel like a deskilling or a rejection of their professional judgment.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teachers_professional_identity, beneficiary,
    organized, biographical, identity_locked, local).

% These students lack the print-rich environments and background knowledge that whole language assumes for natural phonics development. They struggle to acquire foundational decoding skills, leading to reading difficulties and academic setbacks.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support, payer,
    powerless, immediate, trapped, local).

% Students with specific learning disabilities like dyslexia require explicit, systematic phonics instruction to develop reading skills. The whole language approach's reliance on natural acquisition is particularly harmful for them, often leading to severe and persistent reading failure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia, payer,
    powerless, immediate, trapped, local).

% Observe their children struggling with reading and often seek out alternative instructional methods or private tutoring, incurring significant costs. They are constrained by the school system's adopted pedagogy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Conduct research on reading acquisition, often finding strong evidence for the necessity of explicit phonics instruction. Their findings frequently contradict the core tenets of whole language, but their influence on pedagogical practice can be slow and indirect.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a pedagogical approach that emphasizes reading for meaning and integrates literacy across the curriculum, fostering a love of reading by avoiding rote skill instruction.
% TRANSFER_FUNCTION: Transfers pedagogical authority and professional validation to teachers who prioritize holistic, meaning-based instruction, while transferring the burden of decoding acquisition to students' implicit learning capacities, particularly those with strong prior literacy exposure.
% ABSENT_VOICES: Students who fail to acquire reading skills under this approach, particularly those from low-literacy homes or with learning disabilities, are structurally absent from the pedagogical design process. Their struggles are often attributed to external factors rather than the instructional method itself.
% DISAPPEARANCE_RATIONALE: If the whole language approach and its associated curriculum mandates vanished overnight, schools would rapidly adopt more explicit, systematic phonics instruction, teacher training would shift, and the academic outcomes for many struggling readers would likely improve, fundamentally altering literacy education.
% FOUNDING_PROBLEM: Traditional phonics instruction was often rote, decontextualized, and perceived as demotivating for students, leading to a lack of engagement with reading for meaning.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates maintain that the problem of demotivating, decontextualized instruction is still live. Cognitive scientists and advocates for structured literacy argue that while engagement is important, the core problem of decoding acquisition was not adequately addressed by whole language, and that the pendulum swung too far, creating new problems for many students. Research on the 'reading wars' and longitudinal studies of reading outcomes corroborate the contested status.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the pedagogical approach, while well-intentioned, imposes significant costs on students who do not naturally acquire phonics skills, particularly those from less print-rich home environments or with learning disabilities. Suppression (0.70) is also high, as the professional consensus and curriculum mandates often suppress alternative, more explicit instructional methods. Theater ratio (0.20) is moderate; while there is genuine pedagogical activity, some of it serves to maintain the professional identity of advocates rather than solely focusing on student outcomes. The extractiveness and suppression peaked around 2000-2010 as the 'reading wars' intensified and whole language faced increasing scientific scrutiny, leading to a slight decline as 'balanced literacy' gained traction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates, this is a 'rope' or 'scaffold' that fosters a love of reading and holistic development. From the perspective of struggling students and their parents, it operates as a 'snare' or 'tangled rope' that traps them in a cycle of reading failure. The engine's classification will reflect the latter due to the high extractiveness and suppression on vulnerable populations, despite the claimed benefits for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and teachers whose professional identity is tied to this approach are beneficiaries (low d), as it validates their pedagogical philosophy and autonomy. Students without strong home literacy support and those with dyslexia are clear victims (high d), as the approach fails to provide them with necessary foundational skills. Parents of struggling readers bear costs and are constrained in their options. Cognitive scientists act as observers, providing evidence that often challenges the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to make reading instruction more engaging and meaningful, moving away from rote memorization. While this problem is still 'contested' (some argue it remains relevant), the 'founding_problem_status' being contested, combined with the 'world_rearranges' disappearance verdict and high extractiveness on specific groups, suggests that the constraint has drifted from its original coordination function to one that disproportionately extracts from vulnerable students. The classification as a tangled_rope reflects this hybrid nature, where a genuine coordination function (engagement, meaning-making) is intertwined with asymmetric extraction (failure to teach decoding to all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validity_of_natural_acquisition,
    'Is the claim that phonics skills develop naturally through exposure and context empirically valid for all learners, or only for a subset?',
    'Longitudinal studies comparing reading outcomes of diverse student populations under whole language vs. explicit phonics instruction, particularly focusing on students with varying levels of home literacy support and cognitive profiles.',
    'If natural acquisition is not universal, the constraint''s extractiveness on vulnerable students is higher than currently measured, and its coordination function is weaker, pushing it closer to a snare. If it were universal, extractiveness would be lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_validity_of_natural_acquisition, empirical, 'Empirical basis for the ''natural acquisition'' tenet of whole language.').

omega_variable(
    teacher_identity_vs_student_outcomes,
    'To what extent does the whole language approach persist due to its alignment with teachers'' professional identity, even when evidence suggests it is suboptimal for student outcomes?',
    'Qualitative studies of teacher decision-making, surveys on pedagogical beliefs, and analysis of professional development uptake in response to new research findings. This would involve disentangling professional identity from evidence-based practice.',
    'If professional identity is a primary driver, the ''theater_ratio'' is higher than currently measured, as some pedagogical activity serves to maintain identity rather than optimize learning. This would also increase the ''suppression'' of alternative methods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_identity_vs_student_outcomes, conceptual, 'Role of teacher professional identity in the persistence of whole language.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of explicit phonics instruction structural (curriculum mandates, lack of resources) or internalized (teachers'' beliefs, professional norms)?',
    'Analysis of policy changes vs. teacher practice shifts: if explicit phonics instruction remains suppressed even after policy changes remove structural barriers, reclassify as partially internalized. Surveys of teacher attitudes and beliefs.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — teachers carry the suppression with them after policy changes. This would make the constraint more resilient to external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for explicit phonics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1980, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', representing the whole language approach. It is linked to other pedagogical readings (phonics, balanced literacy, structured literacy) which offer alternative approaches to reading instruction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
