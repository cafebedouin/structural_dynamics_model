% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Systematic Phonics Instruction as Foundational Reading Skill
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics reading' of the contested kernel
 *   'reading acquisition mechanism.' It asserts that explicit, systematic
 *   instruction in grapheme-phoneme correspondence is a foundational,
 *   non-negotiable component of early reading instruction. The claim is that
 *   this is a scaffold: a temporary, high-intensity instructional structure
 *   designed to be internalized by the learner, after which the external
 *   constraint (the scope-and-sequence) becomes unnecessary for that
 *   individual. The constraint carries a sunset clause at the individual
 *   level (once decoding is automatic, the explicit instruction ceases)
 *   though policy-level mandates may persist. Beneficiaries are primarily the
 *   most vulnerable learners; costs fall on teacher autonomy and competing
 *   pedagogical frameworks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.18).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.42).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, scaffold).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Systematic Phonics Instruction as Foundational Reading Skill").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).
narrative_ontology:has_sunset_clause(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf').
narrative_ontology:cs_kernel_codification('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', distributed).
narrative_ontology:cs_authority_grounding('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', practice).
narrative_ontology:cs_interpretation_layer_present('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf').
narrative_ontology:cs_reading_relation('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', foundational, alphabetic_principle_must_be_explicitly_taught).
narrative_ontology:cs_axiom_status(alphabetic_principle_must_be_explicitly_taught, holdable).
narrative_ontology:cs_axiom_grounding('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', alphabetic_principle_must_be_explicitly_taught, empirically_contingent).
narrative_ontology:cs_axiom('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', foundational, systematic_scope_and_sequence_necessary_for_at_risk_learners).
narrative_ontology:cs_axiom_status(systematic_scope_and_sequence_necessary_for_at_risk_learners, holdable).
narrative_ontology:cs_axiom_grounding('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', systematic_scope_and_sequence_necessary_for_at_risk_learners, empirically_contingent).
narrative_ontology:cs_reference_frame('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', pre_reading_wars_whole_language_dominance).
narrative_ontology:cs_drift_state('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', post_nrp_evidence_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5ef8b954-4eaf-4dd9-8ed9-a457f2e0d2bf', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, early_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, dyslexic_learners).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, english_language_learners).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teacher_autonomy_proponents).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, whole_language_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, curriculum_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children learning to read who depend on the instructional method chosen for them. Systematic phonics provides the decoding foundation that prevents later reading failure. They have no exit from the educational system and no voice in curriculum decisions.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, early_readers, beneficiary,
    powerless, biographical, trapped, universal).

% Students who would fall behind without explicit decoding instruction. Systematic phonics disproportionately benefits this group by preventing the cascade of failure that occurs when foundational skills are not explicitly taught.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, universal).

% Learners with dyslexia who require explicit, systematic, cumulative instruction in grapheme-phoneme correspondence. Without this approach, they face near-certain reading failure. The constraint's structure directly addresses their neurocognitive needs.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, dyslexic_learners, beneficiary,
    powerless, biographical, trapped, universal).

% ELL students who benefit from explicit sound-symbol mapping that makes English orthography's patterns visible. Systematic phonics provides a transparent bridge from oral language to written text.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, english_language_learners, beneficiary,
    powerless, biographical, trapped, universal).

% Bear the instructional cost of implementing systematic scope-and-sequence. Lose pedagogical discretion when mandates require fidelity to scripted programs. Exit options: change grade level, move to private school, leave profession — all constrained by certification and employment conditions.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, classroom_teachers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, classroom_teachers, agenda_setter).

% Professional organizations and advocates who view scripted phonics programs as deprofessionalizing teaching. They bear costs in professional identity and intellectual freedom. Can exit by advocating for alternative frameworks, publishing research, or organizing politically.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teacher_autonomy_proponents, payer,
    organized, generational, mobile, national).

% Educators whose professional identity and practice are built on whole language philosophy. The constraint directly threatens their pedagogical framework. Exit requires abandoning a core professional identity — identity_locked rather than merely constrained.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_practitioners, payer,
    organized, generational, identity_locked, national).

% Publishers of systematic phonics programs who gain market share when mandates require specific curricula. Can pivot product lines across reading frameworks — arbitrage-grade exit from any single instructional paradigm.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, global).

% Researchers studying reading acquisition mechanisms. Provide evidence on instructional efficacy but do not bear classroom implementation costs or collect direct benefits from policy adoption.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, literacy_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of ensuring all children acquire the alphabetic principle — the foundational insight that written symbols represent speech sounds — which is not reliably discovered through exposure alone. Without coordination, instructional quality varies wildly and the most vulnerable children fail.
% TRANSFER_FUNCTION: Moves instructional time, teacher discretion, and curriculum authority from open-ended literacy practices to a defined scope-and-sequence of grapheme-phoneme correspondences. Transfers initial cognitive load from students (who would otherwise need to induce patterns) to teachers (who must deliver systematic instruction). Long-term, transfers remediation burden away from the system by preventing reading failure.
% ABSENT_VOICES: Children themselves — the primary beneficiaries — have no voice in curriculum decisions. Parents of struggling readers are often excluded from policy debates until their child is already failing. Community organizations in under-resourced districts lack capacity to engage in reading wars debates.
% DISAPPEARANCE_RATIONALE: If systematic phonics mandates disappeared overnight, instructional practice would immediately fragment. Some teachers would continue systematic instruction; others would revert to implicit approaches. The most vulnerable students — dyslexic learners, ELL students, children from low-literacy homes — would lose the structural guarantee of explicit decoding instruction, and remediation costs would rise sharply within 2-3 years.
% FOUNDING_PROBLEM: By the 1990s, whole language dominance had produced a measurable decline in reading proficiency, especially among disadvantaged students. The National Reading Panel (2000) was convened because the 'reading wars' had left a generation of children without reliable decoding skills. The founding problem was: how to ensure every child, regardless of teacher preparation or home environment, acquires the alphabetic principle?
% FOUNDING_PROBLEM_CORROBORATION: The National Reading Panel (2000) and subsequent meta-analyses (e.g., NRP, 2000; Castles et al., 2018; Seidenberg, 2017) corroborate from outside the benefiting parties that systematic phonics is necessary for at-risk populations. However, whole language and balanced literacy advocates (Goodman, 1986; Calkins, 2020 revisions) contest whether the founding problem was ever accurately diagnosed, arguing the decline reflected broader social factors, not instructional method.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).
:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.18) reflects that the constraint's primary operation is coordination — solving a genuine collective action problem in literacy instruction. The extraction that exists comes from narrowing teacher discretion and marginalizing whole language practitioners. Suppression (0.42) is moderate: mandates require fidelity to scope-and-sequence, and competing approaches are disadvantaged in policy and funding, but alternatives are not illegal — teachers can supplement, and private/charter schools may choose other methods. Theater ratio is low (0.12): the instructional activity is genuinely functional, not performative. Accessibility collapse (0.35) is moderate: alternatives (whole language, balanced literacy) persist and are actively practiced, but the evidence base makes them increasingly difficult to defend for at-risk populations. Resistance (0.58) is substantial: the reading wars continue, with organized opposition from teacher autonomy advocates and whole language adherents.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (especially dyslexic_learners), this constraint appears as a mountain — a necessary condition for literacy that should not be contested. From the victim seats (whole_language_practitioners), it appears as a snare — an ideologically driven mandate that suppresses a valid pedagogical tradition. From the teacher seat, it is a tangled rope: genuine coordination benefit (students learn to decode) combined with extraction (loss of professional judgment). The engine computes these per-seat divergences from the structural data; the claimed scaffold type reflects the authoring seat's judgment that the constraint's *telos* is transitional skill-building, not permanent control.
 *
 * DIRECTIONALITY LOGIC:
 *   Early readers, struggling readers, dyslexic learners, and ELL students are structural beneficiaries (d near 0.0): the constraint subsidizes their learning by providing the explicit instruction they cannot induce. Classroom teachers are payers (d ~0.6-0.7): they bear implementation costs and lose discretion, though some gain efficacy. Teacher autonomy proponents and whole language practitioners are victims (d ~0.8-0.9): the constraint directly undermines their professional framework and identity. Curriculum publishers are beneficiaries (d ~0.1-0.2) with arbitrage exit. Literacy researchers are analytical observers (d=0.5). The identity_locked exit for whole_language_practitioners captures the professional identity fusion that makes exit psychologically and professionally costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by explicitly declaring its sunset clause at the individual level (decoding automaticity ends the need for explicit phonics instruction). However, policy-level mandates often lack sunset provisions, creating mandatrophy risk: the temporary scaffold becomes a permanent requirement even after its function is fulfilled for individual learners. The founding problem (ensuring universal decoding acquisition) remains contested — some argue it is solved by the evidence base, others argue it was misdiagnosed. The corroboration from outside the benefiting parties (NRP, independent meta-analyses) supports the coordination function, but the persistence of whole language as a live alternative indicates the mandate has not achieved uncontested legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is ''reading acquisition mechanism'' a single kernel with competing readings, or are these structurally distinct constraints with different ε values?',
    'Apply the ε-invariance test: if measuring the constraint via phonics outcomes yields low ε but measuring via teacher autonomy yields high ε, they are different constraints. Decompose into separate stories if ε varies by observable.',
    'If separate constraints, the phonics reading''s claimed scaffold type may hold; if single kernel, the extraction from teacher autonomy must be integrated into one ε, potentially shifting classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel decomposition follows the BGS pattern (one label, multiple constraints) or represents one constraint with multiple measurement angles.').

omega_variable(
    sunset_clause_realization,
    'Does the individual-level sunset clause (decoding automaticity ends explicit instruction) actually operate in practice, or do policy mandates create a de facto permanent constraint?',
    'Longitudinal classroom observation: track whether teachers continue systematic phonics instruction after students achieve decoding automaticity, and whether policy requires it.',
    'If sunset is not realized, the constraint drifts from scaffold toward piton (theatrical maintenance of a fulfilled function) or snare (permanent extraction of teacher discretion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_realization, empirical, 'Whether the scaffold''s transitional nature is honored in practice or overridden by institutional inertia.').

omega_variable(
    identity_locked_vs_constrained_exit,
    'Is whole_language_practitioners'' exit_options correctly characterized as identity_locked, or is it merely constrained (professional retraining possible but costly)?',
    'Survey whole language practitioners who have adopted systematic phonics: was the transition primarily technical (learning new methods) or identity-constitutive (abandoning professional self-concept)?',
    'If identity_locked is accurate, their directionality d is higher (near 0.9), amplifying effective extraction. If constrained, d is lower (~0.7), reducing the extraction signal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_vs_constrained_exit, empirical, 'Whether professional identity fusion with whole language creates genuine identity lock or merely high switching costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2000, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2005, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2010, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2015, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2020, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_tr_t2025, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2000, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2005, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2010, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2015, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2015, 0.17).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2020, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2020, 0.18).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_be_t2025, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2000, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2005, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2010, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2015, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2020, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(reading_acquisition_mechanism__phonics_reading_su_t2025, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__phonics_reading, 0.02).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the reading_acquisition_mechanism kernel family. The phonics_reading asserts explicit systematic instruction as foundational (low ε, scaffold). The whole_language_reading asserts implicit emergence from text engagement (different ε, different beneficiary/victim structure). The balanced_literacy_reading asserts integration of both (intermediate ε, different coordination function). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__phonics_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
