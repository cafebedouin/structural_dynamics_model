% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Systematic Phonics Decoding Primacy in Reading Instruction
 *   domain: education/policy/cognitive_science
 *
 * SUMMARY:
 *   This constraint story models the phonics-decoding-primacy reading of the
 *   contested reading-acquisition legitimacy kernel. Under this reading,
 *   reading is fundamentally a decoding process, and legitimate instruction
 *   must make the alphabetic principle explicit through systematic,
 *   sequential phonics. The constraint operates through state curriculum
 *   mandates, teacher preparation standards, early screening assessments, and
 *   the delegitimization of whole-language and balanced-literacy
 *   alternatives. It is claimed as coordination (ensuring all children
 *   receive evidence-based decoding instruction, particularly benefiting
 *   students with dyslexia) but extracts professional autonomy from teachers,
 *   marginalizes whole-language educators, and channels public education
 *   resources toward phonics curriculum vendors and assessment providers.
 *
 * KEY AGENTS:
 *   - science_of_reading_researchers: Primary agenda-setter (institutional/analytical) â defines what counts as evidence-based reading instruction
 *   - state_policy_makers: Secondary agenda-setter (institutional/constrained) â mandates phonics screening and curriculum standards
 *   - students_with_dyslexia: Primary beneficiary (powerless/trapped) â gains from explicit decoding instruction
 *   - phonics_curriculum_publishers: Secondary beneficiary (organized/mobile) â captures revenue from mandated curricula
 *   - classroom_teachers: Primary payer (moderate/constrained) â loses pedagogical autonomy to scripted programs
 *   - whole_language_educators: Secondary payer (organized/constrained) â professional expertise delegitimized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.58).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Systematic Phonics Decoding Primacy in Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education/policy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '09a459fc-f371-4edc-af12-0733b21a37d0').
narrative_ontology:cs_kernel_codification('09a459fc-f371-4edc-af12-0733b21a37d0', formalized).
narrative_ontology:cs_authority_grounding('09a459fc-f371-4edc-af12-0733b21a37d0', expertise).
narrative_ontology:cs_interpretation_layer_present('09a459fc-f371-4edc-af12-0733b21a37d0').
narrative_ontology:cs_reading_relation('09a459fc-f371-4edc-af12-0733b21a37d0', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('09a459fc-f371-4edc-af12-0733b21a37d0', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_reading_relation('09a459fc-f371-4edc-af12-0733b21a37d0', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('09a459fc-f371-4edc-af12-0733b21a37d0', foundational, reading_is_decoding_process).
narrative_ontology:cs_axiom_status(reading_is_decoding_process, holdable).
narrative_ontology:cs_axiom_grounding('09a459fc-f371-4edc-af12-0733b21a37d0', reading_is_decoding_process, empirically_contingent).
narrative_ontology:cs_axiom('09a459fc-f371-4edc-af12-0733b21a37d0', foundational, alphabetic_principle_must_be_explicit).
narrative_ontology:cs_axiom_status(alphabetic_principle_must_be_explicit, holdable).
narrative_ontology:cs_axiom_grounding('09a459fc-f371-4edc-af12-0733b21a37d0', alphabetic_principle_must_be_explicit, empirically_contingent).
narrative_ontology:cs_reference_frame('09a459fc-f371-4edc-af12-0733b21a37d0', systematic_explicit_phonics_framework).
narrative_ontology:cs_drift_state('09a459fc-f371-4edc-af12-0733b21a37d0', contemporary_policy_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('09a459fc-f371-4edc-af12-0733b21a37d0', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, assessment_vendors).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_educators).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_alternative_strengths).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and disseminate research establishing the alphabetic principle and systematic phonics as the evidence-based standard. Their professional credibility, grant funding, and policy influence depend on the acceptance of this framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading_researchers, agenda_setter,
    institutional, generational, analytical, national).

% Mandate phonics screening checks and explicit phonics instruction in state standards and legislation. Derive political legitimacy from 'following the science' and improving literacy metrics.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, state_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Benefit disproportionately from explicit, systematic phonics instruction that builds decoding skills they would not acquire through implicit or meaning-first approaches.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, national).

% Sell scripted phonics programs, decodable texts, and assessment tools to districts mandated to adopt systematic phonics. Revenue scales directly with policy adoption and mandate enforcement.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Provide the decoding assessments and screening tools required by phonics mandates. Benefit from the standardization of early reading measurement around phonemic awareness and decoding fluency.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, assessment_vendors, beneficiary,
    organized, biographical, mobile, national).

% Required to follow scripted phonics curricula and pacing guides, reducing professional judgment about instructional methods. Face accountability pressure through decoding assessments that determine program fidelity and evaluation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, classroom_teachers, payer,
    moderate, biographical, constrained, national).

% Professional expertise and preferred methods are delegitimized by the phonics mandate. Their training, materials, and professional networks lose institutional standing, funding, and access to classrooms.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_educators, payer,
    organized, generational, constrained, national).

% Learners who might thrive with meaning-first, contextual, or whole-word approaches are constrained by the phonics-only legitimacy structure. Their reading difficulties are attributed to instruction failure rather than model misfit.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_alternative_strengths, payer,
    powerless, biographical, trapped, national).

% Attempt to integrate phonics with authentic literature and meaning-making but are often excluded from policy tables where 'the science of reading' is defined narrowly as systematic phonics.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_practitioners, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes early reading instruction around a replicable, evidence-based method; reduces variance in instructional quality across classrooms; ensures students with decoding difficulties receive explicit, systematic support rather than relying on implicit learning or teacher intuition.
% TRANSFER_FUNCTION: Moves pedagogical authority from classroom teachers and whole-language educators to phonics researchers, curriculum publishers, and state assessment regimes; moves financial resources from public education budgets to phonics program vendors and assessment providers.
% ABSENT_VOICES: Balanced literacy practitioners and whole-language educators are often excluded from policy tables where 'science of reading' is defined; parents seeking alternative instructional models for children who struggle with phonics; students who cannot articulate that the instructional model misfits them.
% DISAPPEARANCE_RATIONALE: If the phonics-decoding-primacy legitimacy structure vanished, school districts would revert to mixed or locally-determined methods, the market for scripted phonics curricula would contract, teacher training programs would rebalance toward broader pedagogical methods, and early reading assessment would diversify beyond decoding metrics.
% FOUNDING_PROBLEM: High rates of reading failure, particularly among disadvantaged learners and those with dyslexia, under implicit or unsystematic instructional approaches; need for reliable, reproducible early reading instruction that does not depend on individual teacher intuition.
% FOUNDING_PROBLEM_CORROBORATION: Science of reading researchers and dyslexia advocacy organizations attest the problem remains live and is addressed by phonics. Balanced literacy advocates and some literacy education researchers contend the founding problem has been partially solved by existing phonics integration and that the current arrangement persists as an overcorrection; independent meta-analyses and international literacy comparisons provide mixed corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint channels financial and professional resources toward phonics-specific infrastructure while constraining alternative methods. Suppression (0.58) reflects the active delegitimization of whole-language and the narrowing of 'science of reading' to phonics in policy discourse. Theater ratio (0.40) captures performative compliance â schools purchasing decodable texts while continuing mixed methods, and professional development that rehearses phonics rhetoric without changing practice. Accessibility collapse (0.60) indicates that once the phonics frame dominates policy, alternative instructional models become difficult for parents or teachers to access. Resistance (0.48) reflects ongoing pushback from balanced-literacy advocates, some teacher educators, and parents of students misfitted by phonics-first approaches. The temporal series show extraction and enforcement intensifying as phonics moved from research consensus to policy mandate over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (researchers, state officials) experience the constraint as genuine coordination â a necessary standardization to prevent reading failure. The payer seats (teachers, whole-language educators, misfitted students) experience it as extraction of autonomy and misalignment. The beneficiary seats (dyslexic students, publishers) experience it as resource provision or market opportunity. The engine should compute low directionality for students_with_dyslexia (subsidized by the constraint) and high directionality for teachers and whole-language educators (targets of extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (students_with_dyslexia, phonics_curriculum_publishers, assessment_vendors) derive direct benefit â low directionality. Payers (classroom_teachers, whole_language_educators, students_with_alternative_strengths) bear costs of constrained practice and misfit â high directionality. Agenda-setters (science_of_reading_researchers, state_policy_makers) administer the constraint; researchers may partially benefit from epistemic authority but state officials are constrained by political accountability. No override needed: structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â high reading failure under unsystematic instruction â remains contested. While phonics addresses decoding deficits, the constraint's persistence as a narrowing mandate (rather than one tool among many) suggests partial mandatrophy: the arrangement continues expanding even as the original problem specification (how to teach all children to read) would benefit from a more integrated approach. The temporal measurements show extraction rising faster than coordination value, a drift signature consistent with tangled_rope rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonics_universality_boundary,
    'Does the empirical evidence for systematic phonics support its primacy for all learners and all developmental stages, or primarily for specific subgroups (e.g., dyslexic readers, initial code-breaking stages)?',
    'Longitudinal comparative studies tracking diverse learner populations through mixed-method instruction versus phonics-only instruction, with outcome measures beyond decoding (comprehension, motivation, secondary school literacy).',
    'If phonics primacy is subgroup-specific, the universal mandate extracts from learners who would benefit from alternative entry points, strengthening the snare/tangled_rope classification. If universal, the coordination function is broader and extraction lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_universality_boundary, empirical, 'Boundary of empirical support for phonics universality').

omega_variable(
    policy_mandate_vs_professional_judgment,
    'Is the extraction from teacher autonomy an unavoidable cost of standardizing effective instruction, or does the policy layer add unnecessary coercion beyond what the evidence requires?',
    'Comparison of jurisdictions with phonics guidance (non-mandated) versus phonics mandates: literacy outcomes, teacher retention, and curriculum fidelity.',
    'If outcomes are equivalent under guidance, the mandate layer is extractive overhead. If mandates produce superior outcomes, the enforcement is coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_mandate_vs_professional_judgment, empirical, 'Whether policy mandate adds extraction beyond evidence-based guidance').

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the phonics_decoding_primacy reading of the reading_acquisition_legitimacy kernel. How would classification change if the whole_language_meaning_primacy or balanced_literacy_integration reading were adopted as the governing framework?',
    'Cross-reading analysis: the same structural arrangement (state curriculum standards) under different kernel readings produces different beneficiary/victim distributions and different epsilon values.',
    'The sibling reading would reverse several payer/beneficiary roles and alter the extraction vector. This reading''s classification as tangled_rope is specific to the phonics-primacy instantiation; under a balanced reading, the constraint might compute as rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committers on kernel reading contestation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phonics_decode_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(phonics_decode_tr_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 8, 0.22).
narrative_ontology:measurement(phonics_decode_tr_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 16, 0.3).
narrative_ontology:measurement(phonics_decode_tr_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 24, 0.36).
narrative_ontology:measurement(phonics_decode_tr_t32, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 32, 0.4).

% Extraction over time
narrative_ontology:measurement(phonics_decode_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(phonics_decode_be_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(phonics_decode_be_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(phonics_decode_be_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(phonics_decode_be_t32, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 32, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(phonics_decode_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(phonics_decode_su_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(phonics_decode_su_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(phonics_decode_su_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(phonics_decode_su_t32, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 32, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reading_acquisition_legitimacy kernel, which decomposes into multiple structurally distinct claims about how reading should be taught. Each reading produces a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
