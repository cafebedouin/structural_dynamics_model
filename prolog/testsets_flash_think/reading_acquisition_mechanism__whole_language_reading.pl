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
 *   human_readable: Whole Language Reading Pedagogy
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'Whole Language' approach to reading
 *   acquisition, which posits that decoding skills emerge implicitly from
 *   meaningful engagement with authentic texts, rather than requiring
 *   explicit, systematic instruction. It is one reading of the broader
 *   'reading_acquisition_mechanism' kernel. The approach emphasizes immersion
 *   and context, aiming to foster a love of reading. However, its
 *   effectiveness, particularly for struggling readers, has been a subject of
 *   intense debate and scientific scrutiny.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.8).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.7).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Pedagogy").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '8078d91c-e7e8-4a6e-a224-0a7fc2548068').
narrative_ontology:cs_kernel_codification('8078d91c-e7e8-4a6e-a224-0a7fc2548068', formalized).
narrative_ontology:cs_authority_grounding('8078d91c-e7e8-4a6e-a224-0a7fc2548068', practice).
narrative_ontology:cs_interpretation_layer_present('8078d91c-e7e8-4a6e-a224-0a7fc2548068').
narrative_ontology:cs_reading_relation('8078d91c-e7e8-4a6e-a224-0a7fc2548068', reading_acquisition_mechanism__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('8078d91c-e7e8-4a6e-a224-0a7fc2548068', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('8078d91c-e7e8-4a6e-a224-0a7fc2548068', foundational, reading_is_natural_language_process).
narrative_ontology:cs_axiom_status(reading_is_natural_language_process, holdable).
narrative_ontology:cs_axiom_grounding('8078d91c-e7e8-4a6e-a224-0a7fc2548068', reading_is_natural_language_process, conventional).
narrative_ontology:cs_axiom('8078d91c-e7e8-4a6e-a224-0a7fc2548068', foundational, decoding_emerges_implicitly).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly, holdable).
narrative_ontology:cs_axiom_grounding('8078d91c-e7e8-4a6e-a224-0a7fc2548068', decoding_emerges_implicitly, empirically_contingent).
narrative_ontology:cs_reference_frame('8078d91c-e7e8-4a6e-a224-0a7fc2548068', natural_language_acquisition_model).
narrative_ontology:cs_drift_state('8078d91c-e7e8-4a6e-a224-0a7fc2548068', contemporary_cognitive_science_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('8078d91c-e7e8-4a6e-a224-0a7fc2548068', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and defend the whole language approach, benefiting from its adoption in curricula and the professional identity it fosters. They often dismiss critiques as misinterpretations or oversimplifications.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from pedagogical autonomy and a less prescriptive curriculum, which aligns with a philosophy of natural learning. They may face challenges with struggling students but are often trained within this paradigm.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, teachers, beneficiary,
    organized, biographical, constrained, local).

% Bear the primary cost of this method, as their decoding skills do not emerge implicitly, leading to significant reading difficulties, academic setbacks, and a need for intensive, often costly, remediation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Bear the emotional and financial costs of their children's reading difficulties, often seeking external tutoring or advocating for alternative instructional methods within a system resistant to change.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Advocate for explicit, systematic phonics instruction, often marginalized or dismissed by whole language proponents within educational institutions, despite growing scientific evidence supporting their claims.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, phonics_advocates, excluded,
    organized, generational, constrained, national).

% Research the cognitive mechanisms of reading acquisition, often finding evidence that contradicts the implicit decoding premise of whole language, but their findings may be slow to influence pedagogical practice.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, cognitive_scientists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates teachers and students around a holistic, meaning-focused approach to reading, fostering a love of literature and natural language acquisition by immersing students in authentic texts.
% TRANSFER_FUNCTION: Transfers pedagogical autonomy to teachers and a focus on meaning-making to students, while implicitly transferring the burden of decoding skill acquisition to the students themselves, often leading to significant remediation costs for struggling learners.
% ABSENT_VOICES: Advocates for explicit, systematic phonics instruction, and parents of children who fail to learn to read under this method, whose concerns are often dismissed as anecdotal or attributed to other factors rather than the pedagogical approach itself.
% DISAPPEARANCE_RATIONALE: If this pedagogical approach vanished overnight, educational systems would rapidly shift to more explicit, systematic methods, teacher training would change, and the landscape of early literacy instruction would be fundamentally altered, likely reducing the number of struggling readers requiring intensive remediation.
% FOUNDING_PROBLEM: To counter overly mechanistic and decontextualized phonics instruction that was perceived to stifle a love of reading and natural language development, and to promote a more engaging, meaning-rich approach.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates attest the problem of decontextualized instruction is still live. Cognitive scientists and phonics advocates attest that while the original problem was real, the whole language solution created new, more severe problems, and that the founding problem is largely superseded by current understanding of reading science; research from independent educational bodies and cognitive science journals supports the shifted-function reading.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.8) is high due to the significant long-term remediation costs borne by struggling readers and their families, whose decoding skills do not implicitly emerge. Suppression (0.7) reflects the institutional entrenchment of this pedagogy, which often marginalizes or actively resists alternative, explicit phonics-based methods. The theater ratio (0.4) indicates that while 'meaningful engagement' is a genuine goal, a portion of the pedagogical activity becomes performative, failing to deliver foundational skills for all students. Resistance (0.7) is high, driven by parents, researchers, and advocates for evidence-based literacy instruction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates and many teachers, this approach is a beneficial 'rope' that fosters natural language development and a love of reading. However, from the perspective of struggling readers and their parents, it operates as a 'snare' or 'tangled_rope', extracting significant costs in terms of academic failure and remediation, while suppressing effective alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and many teachers are beneficiaries, gaining pedagogical autonomy and alignment with a preferred philosophy (low directionality). Struggling readers and their parents are clear targets, bearing the costs of the method's shortcomings (high directionality). Phonics advocates are structurally excluded, their alternative approach suppressed by the dominant paradigm. Cognitive scientists act as observers, providing analytical input that often challenges the constraint's foundational claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of countering overly mechanistic phonics instruction is now contested. While the original concern was valid, the whole language approach's failure to adequately address decoding for all students has led to new problems, suggesting a potential mandatrophy where the solution has outlived its utility for a significant portion of the population. The persistence of the constraint, despite accumulating evidence of its ineffectiveness for many, points to institutional inertia and identity-lock among proponents, rather than a live, universally beneficial mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_decoding_scientific_validity,
    'Is the claim that decoding skills emerge implicitly from exposure scientifically valid for all learners, or only for a subset?',
    'Further longitudinal studies in cognitive science and educational psychology, particularly those tracking diverse learner populations and neurological markers of reading acquisition.',
    'If implicit decoding is found to be insufficient for a significant portion of learners, the constraint''s ''rope'' framing collapses, strengthening its ''snare'' or ''tangled_rope'' classification due to the unaddressed costs imposed on those learners.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_decoding_scientific_validity, empirical, 'Empirical validity of the implicit decoding hypothesis.').

omega_variable(
    pedagogical_autonomy_vs_student_outcomes,
    'To what extent does the value placed on teacher pedagogical autonomy within this framework conflict with the need for evidence-based practices that ensure universal reading proficiency?',
    'Policy analysis comparing educational systems with high teacher autonomy in literacy instruction versus those with mandated evidence-based curricula, assessing student literacy outcomes across socioeconomic strata.',
    'If high autonomy correlates with poorer outcomes for vulnerable students, the constraint''s coordination function (for teachers) would be seen as directly enabling extraction (from students), reinforcing a ''tangled_rope'' classification and potentially shifting it towards ''snare'' if the harm is severe and systemic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_autonomy_vs_student_outcomes, conceptual, 'Trade-off between teacher autonomy and universal student outcomes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative literacy methods structural (institutional policies, curriculum mandates) or internalized (teachers'' professional identity, ideological commitment to whole language)?',
    'Analysis of policy changes in jurisdictions that remove structural barriers to alternative methods: if suppression persists, it suggests a strong internalized component. Teacher surveys on pedagogical beliefs and resistance to adopting new methods.',
    'If internalized suppression is dominant, the constraint''s effective suppression is higher than structural measures suggest, as resistance to change comes from within the pedagogical community itself, making reform more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative pedagogies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('whole_language_reading') of the 'reading_acquisition_mechanism' kernel, which also includes 'phonics_reading' and 'balanced_literacy_reading'. Each reading represents a distinct pedagogical approach with different structural properties and outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
