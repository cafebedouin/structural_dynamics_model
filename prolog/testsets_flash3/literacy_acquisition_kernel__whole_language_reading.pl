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
 *   This constraint represents the 'whole language' reading of the literacy
 *   acquisition kernel. It posits that reading emerges naturally from
 *   meaningful engagement with connected text, with phonics skills developing
 *   implicitly. Explicit decoding instruction is deemed unnecessary and
 *   potentially harmful to motivation. The constraint's extractiveness is
 *   borne by students, particularly those from print-poor environments, who
 *   do not naturally acquire the necessary foundational skills. It benefits
 *   teachers' professional autonomy and identity, aligning with a progressive
 *   educational philosophy.
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
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Pedagogy").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '50de604f-b769-4453-8e40-c4d41b66cbee').
narrative_ontology:cs_kernel_codification('50de604f-b769-4453-8e40-c4d41b66cbee', distributed).
narrative_ontology:cs_authority_grounding('50de604f-b769-4453-8e40-c4d41b66cbee', practice).
narrative_ontology:cs_interpretation_layer_present('50de604f-b769-4453-8e40-c4d41b66cbee').
narrative_ontology:cs_reading_relation('50de604f-b769-4453-8e40-c4d41b66cbee', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('50de604f-b769-4453-8e40-c4d41b66cbee', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('50de604f-b769-4453-8e40-c4d41b66cbee', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('50de604f-b769-4453-8e40-c4d41b66cbee', foundational, reading_is_natural_language_process).
narrative_ontology:cs_axiom_status(reading_is_natural_language_process, holdable).
narrative_ontology:cs_axiom_grounding('50de604f-b769-4453-8e40-c4d41b66cbee', reading_is_natural_language_process, deontological).
narrative_ontology:cs_axiom('50de604f-b769-4453-8e40-c4d41b66cbee', secondary, explicit_phonics_harms_motivation).
narrative_ontology:cs_axiom_status(explicit_phonics_harms_motivation, holdable).
narrative_ontology:cs_axiom_grounding('50de604f-b769-4453-8e40-c4d41b66cbee', explicit_phonics_harms_motivation, empirically_contingent).
narrative_ontology:cs_reference_frame('50de604f-b769-4453-8e40-c4d41b66cbee', child_centered_meaning_making).
narrative_ontology:cs_drift_state('50de604f-b769-4453-8e40-c4d41b66cbee', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('50de604f-b769-4453-8e40-c4d41b66cbee', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teachers_seeking_autonomy).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_lacking_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, parents_seeking_explicit_instruction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and defend the whole language approach, influencing curriculum development and teacher training. They benefit from the professional identity and academic careers built around this paradigm.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, constrained, national).

% Embrace whole language as it aligns with their desire for professional judgment and less prescriptive teaching methods, fostering a more 'natural' learning environment. They benefit from the perceived freedom and alignment with progressive educational philosophies.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teachers_seeking_autonomy, beneficiary,
    moderate, biographical, mobile, local).

% Are disproportionately harmed by the lack of explicit phonics instruction, as they do not acquire foundational decoding skills naturally through exposure. Their access to literacy is constrained by their home environment, which the pedagogy fails to compensate for.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_lacking_home_literacy_support, payer,
    powerless, immediate, trapped, local).

% Advocate for more explicit phonics instruction, often observing their children struggle with whole language methods. They bear the cost of seeking supplemental tutoring or advocating for curriculum changes, facing resistance from entrenched pedagogical views.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_seeking_explicit_instruction, payer,
    organized, biographical, constrained, local).

% Conduct studies on reading acquisition, often finding strong evidence for the necessity of explicit phonics. Their findings frequently contradict whole language tenets, but their influence on pedagogical practice can be slow and contested.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, cognitive_science_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates educational practice around a holistic view of literacy, emphasizing meaning-making and engagement over decontextualized skill instruction, fostering a consistent pedagogical approach within schools and districts.
% TRANSFER_FUNCTION: Transfers pedagogical authority and professional autonomy to teachers, while transferring the burden of acquiring foundational decoding skills to students' natural exposure and home environments, often at the expense of those lacking such support.
% ABSENT_VOICES: Students who fail to acquire reading skills under this method, particularly those from disadvantaged backgrounds, are structurally absent from the pedagogical design process. Their struggles are often attributed to individual deficits rather than instructional shortcomings.
% DISAPPEARANCE_RATIONALE: If whole language pedagogy vanished overnight, there would be a rapid shift towards more explicit and systematic phonics instruction, curriculum materials would change, and teacher training would be reoriented. The educational landscape for early literacy would fundamentally reorganize.
% FOUNDING_PROBLEM: To counter overly mechanistic and decontextualized phonics instruction that alienated students from the joy of reading, and to promote a more natural, meaning-centered approach to literacy.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates attest the problem of decontextualized instruction is still live. Cognitive science researchers and parents seeking explicit instruction attest that while the original problem was valid, the pendulum swung too far, creating new problems of foundational skill deficits, and the current arrangement persists due to institutional inertia and professional identity rather than solving a live problem.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.65) is high because the pedagogical approach, while well-intentioned, fails to provide essential skills to a significant portion of students, effectively extracting their potential for literacy. Suppression (0.70) is high due to the institutional entrenchment of whole language, making it difficult for alternative methods to gain traction or for parents to advocate for explicit instruction. Theater ratio (0.20) is moderate, as there is genuine pedagogical activity, but a portion of it serves to maintain the paradigm despite mounting evidence against its efficacy for all learners. The temporal measurements show a rise in extractiveness and suppression as the pedagogical approach became more entrenched and its limitations more apparent, followed by a slight dip as resistance grew.
 *
 * PERSPECTIVAL GAP:
 *   Teachers and advocates perceive this as a beneficial, child-centered approach that fosters a love of reading. Parents and researchers, particularly those observing struggling learners, perceive it as an extractive system that fails to equip all students with essential skills, relying on an unproven 'natural' acquisition process.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and teachers seeking autonomy are beneficiaries (low d), as the approach supports their professional identity and pedagogical preferences. Students lacking home literacy support and parents seeking explicit instruction are victims (high d), bearing the costs of inadequate foundational instruction. Cognitive science researchers act as observers, providing evidence that often challenges the constraint's premises.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to foster a love of reading and avoid mechanistic instruction is still live, but its method has arguably outlived its function for a significant population of learners. The classification as a Tangled Rope reflects that it genuinely coordinates a pedagogical approach (benefiting teachers' autonomy) but does so with significant, asymmetric extraction from vulnerable students, requiring active enforcement to maintain against scientific evidence and parental resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_acquisition_validity,
    'To what extent do phonics skills truly develop ''naturally'' through exposure and context for all learners, particularly those from diverse linguistic and socioeconomic backgrounds?',
    'Longitudinal studies tracking reading outcomes in diverse populations under whole language instruction versus explicit phonics instruction, controlling for home literacy environment.',
    'If natural acquisition is insufficient for a significant portion of learners, the constraint''s extractiveness and suppression would be reclassified as higher, supporting a Snare classification. If it proves sufficient for most, the Rope classification would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_acquisition_validity, empirical, 'Empirical validity of the ''natural acquisition'' premise.').

omega_variable(
    pedagogical_identity_vs_evidence,
    'Is the persistence of whole language pedagogy primarily driven by its alignment with teachers'' professional identity and autonomy, or by a genuine belief in its universal efficacy supported by evidence?',
    'Analysis of teacher training curricula, professional development uptake, and the framing of research findings within educational psychology journals, alongside surveys of teacher beliefs and practices.',
    'If professional identity is the primary driver, the theater_ratio would be higher, indicating performative maintenance of a preferred identity despite conflicting evidence. This would push the classification closer to a Piton or a more entrenched Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_identity_vs_evidence, conceptual, 'Role of professional identity in maintaining pedagogical practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional entrenchment, curriculum mandates) or internalized (teachers'' belief in the method, resistance to change)?',
    'Post-policy-change suppression trajectory: if suppression of alternative methods persists after institutional barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the pedagogical community carries the suppression with them after policy shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in pedagogical adoption.').


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
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', focusing on the whole language approach. It is linked to other readings (phonics, balanced, structured literacy) which represent alternative pedagogical approaches to the same core problem of teaching reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
