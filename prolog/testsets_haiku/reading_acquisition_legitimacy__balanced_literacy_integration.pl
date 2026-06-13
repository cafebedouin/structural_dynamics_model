% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration Instructional Model
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   The balanced literacy reading of the reading-acquisition legitimacy
 *   kernel declares that reading development requires both explicit decoding
 *   instruction (phonics) and engagement with meaning-bearing authentic
 *   literature. This reading emerged in response to the reading wars — the
 *   ideological conflict between phonics-first and whole-language advocates.
 *   Balanced literacy positions itself as empirically-grounded synthesis:
 *   cognitive science shows both decoding automaticity and comprehension
 *   engagement are necessary; therefore, legitimate instruction allocates
 *   time to both, and professional teachers assess individual students' gaps
 *   and adjust. The constraint is CLAIMED as rope (coordination of two
 *   necessary functions) and MEASURED as substantially extractive, moderately
 *   suppressive — the authored metrics describe the actual operation (teacher
 *   time diversion, material purchasing patterns, intervention industry
 *   expansion) as moderately extractive, not because the coordination
 *   function is false but because extraction rides on top of it. The kernel
 *   contest is live: phonics-decoding-primacy and
 *   whole-language-meaning-primacy both contest this reading's balance as
 *   either too much of the wrong thing or not enough of the right thing.
 *
 * KEY AGENTS:
 *   - classroom_teachers (agenda_setter, moderate power) — allocate time between phonics and guided reading; their judgment about student needs is structurally central
 *   - struggling_readers (beneficiary, powerless, identity-locked) — receive both phonics intervention and meaning-driven reading; their self-concept as readers is fused with the constraint structure
 *   - reading_intervention_industry (beneficiary, institutional) — markets both phonics and guided reading systems under the balanced framework
 *   - explicit_phonics_advocates (excluded, organized) — contest the time allocation and teacher authority to de-prioritize phonics
 *   - whole_language_practitioners (excluded, organized) — contest the phonics component as unnecessary interference
 *   - school_district_administrators (payer, institutional) — implement and fund the dual-materials and dual-training model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.38).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.29).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Instructional Model").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, '67c8ed9c-d20d-42b7-9fe2-300228ab5925').
narrative_ontology:cs_kernel_codification('67c8ed9c-d20d-42b7-9fe2-300228ab5925', distributed).
narrative_ontology:cs_authority_grounding('67c8ed9c-d20d-42b7-9fe2-300228ab5925', expertise).
narrative_ontology:cs_interpretation_layer_present('67c8ed9c-d20d-42b7-9fe2-300228ab5925').
narrative_ontology:cs_reading_relation('67c8ed9c-d20d-42b7-9fe2-300228ab5925', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('67c8ed9c-d20d-42b7-9fe2-300228ab5925', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('67c8ed9c-d20d-42b7-9fe2-300228ab5925', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('67c8ed9c-d20d-42b7-9fe2-300228ab5925', foundational, reading_multifactorial_decoding_and_meaning_both_necessary).
narrative_ontology:cs_axiom_status(reading_multifactorial_decoding_and_meaning_both_necessary, holdable).
narrative_ontology:cs_axiom_grounding('67c8ed9c-d20d-42b7-9fe2-300228ab5925', reading_multifactorial_decoding_and_meaning_both_necessary, empirically_contingent).
narrative_ontology:cs_axiom('67c8ed9c-d20d-42b7-9fe2-300228ab5925', foundational, teacher_professional_judgment_legitimate_on_student_needs).
narrative_ontology:cs_axiom_status(teacher_professional_judgment_legitimate_on_student_needs, holdable).
narrative_ontology:cs_axiom_grounding('67c8ed9c-d20d-42b7-9fe2-300228ab5925', teacher_professional_judgment_legitimate_on_student_needs, instrumental).
narrative_ontology:cs_reference_frame('67c8ed9c-d20d-42b7-9fe2-300228ab5925', reading_science_synthesis_decoding_and_meaning_integrated).
narrative_ontology:cs_drift_state('67c8ed9c-d20d-42b7-9fe2-300228ab5925', contemporary_accountability_pressure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('67c8ed9c-d20d-42b7-9fe2-300228ab5925', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, typical_learners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, reading_science_integrationists).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).
:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.38 (moderate, stable across the interval with slight rise then flatline). This is NOT because the coordination function (decoding + meaning together) is false. Rather: the constraint extracts through multiple mechanisms. (1) Material purchasing: districts now buy both decodable readers AND chapter book libraries instead of either/or, expanding vendor revenue; vendors benefit (reading_intervention_industry). (2) Teacher time allocation: time devoted to phonics instruction (30-40 min) is structured time that could otherwise be discretionary; the constraint mandates the allocation. (3) Identity lock: struggling readers internalize a model of themselves as 'students who need both intervention (phonics) and guided reading (meaning)' — the constraint shapes how they understand their own reading development. (4) Exclusion of alternatives: purely systematic phonics or purely literature-immersion approaches are sidelined; the middle position is presented as the only empirically defensible one. Suppression is lower (0.29) because the constraint is not coercive in an enforcement sense — no one is legally compelled; school boards adopt it, teachers implement it, families generally accept it (even if some contest it politically). Accessibility collapse is moderate (0.62) because alternatives (pure phonics curricula, whole-language approaches) remain available outside the constraint but are treated as ideologically illegitimate. Theater ratio is low (0.22) because the dual-function model has real pedagogical content — phonics lessons happen, guided reading happens — but growing portions are performative: demonstrating compliance with balanced frameworks, meeting district metrics on both phonics measures AND comprehension measures, performing pedagogical neutrality in politically contentious environments. The measurement series shows stable-to-slightly-rising extraction and theater, with slight uptick around year 3 (possibly driven by accountability pressure and the need to measure both phonics and comprehension outcomes on standardized assessments).
 *
 * PERSPECTIVAL GAP:
 *   The constraint diverges in its operation across three institutional levels: (1) District/policy level: balanced literacy is mandated as best-practice; materials and PD align to it; phonics-first and whole-language candidates are sidelined. (2) Classroom level: teachers experience it as discretion (they choose when to emphasize phonics vs. meaning) within constraints (both must be present). (3) Student level: struggling readers experience it as diagnosis (they are identified as needing both) and identity (they become 'intervention students'). Across these levels, the constraint's extractiveness operates differently: at the policy level it extracts through vendor expansion and budget redirection; at the classroom level it extracts through increased workload and contested authority; at the student level it extracts through identity lock. No single seat sees the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Classroom teachers are the nominal agenda-setters (they allocate time) but are moderately powered and constrained — they work within district curriculum, state standards, and accountability frameworks. Their directionality is moderate (~0.45–0.55) because they set day-to-day implementation but not the rule itself. Struggling readers are powerless and identity-locked (exit is unthinkable given how reading difficulty becomes part of self-concept); their directionality is high (~0.70–0.80) as targets of the constraint structure, even though they also benefit from the coordination function itself. Typical learners also benefit from the coordination but are less power-constrained; their d is lower (~0.35–0.45). The reading intervention industry is institutional and arbitrage-mobile (they could sell phonics-only or whole-language products elsewhere); their d is low (~0.15–0.25) as a capturer-beneficiary. Explicit phonics advocates are organized and mobile (they can publish, teach elsewhere, influence policy) but are excluded from this constraint's authority structure; their d is moderate-to-high (~0.50–0.65) as targets of exclusion — they bear the cost of legitimacy denial. No directionality override is needed; the derivation from beneficiary/victim + exit options produces accurate values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ideological gridlock between phonics and whole-language camps) was real and is contested as either live or resolved. Balanced literacy's response is to declare the dispute empirically resolvable: both are necessary. However, mandatrophy is avoided because the coordination function is genuine — reading development really does depend on both decoding automaticity and meaning engagement. The constraint does not represent a dead founding function that persists by inertia. The extraction components (material purchasing, time allocation, identity lock) are real and warrant scrutiny, but they ride on a coordination function that remains live. If the founding problem is actually resolved (if phonics-first advocates and whole-language practitioners stopped fighting and adopted an empirically-informed synthesis), then balanced literacy becomes simply the technical implementation of that resolution, and extraction would drop as the constraint lost its ideological work. If the founding problem is still live (if the dispute is actually about resources and authority, not empirical facts), then balanced literacy persists as a compromise arrangement whose extraction continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_mechanisms_sufficiency,
    'Are decoding automaticity and meaning engagement truly BOTH necessary and SUFFICIENT for reading development, or does the balance need adjustment based on learner characteristics (age, prior knowledge, language background)?',
    'Randomized controlled trials comparing phonics-only, whole-language-only, and balanced approaches, stratified by learner characteristics. Meta-analysis of such studies.',
    'If balance is universal-sufficient, the constraint''s allocation stands. If allocation needs to be adaptive (e.g., more phonics for younger children or struggling readers, more meaning for older fluent readers), the balanced model must be restructured to include conditional rules, weakening its claim to straightforward balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_mechanisms_sufficiency, empirical, 'Whether the balanced 50-50 (or 40-60) split is empirically optimal across learner populations.').

omega_variable(
    teacher_judgment_reliability,
    'How reliably can classroom teachers assess their students'' decoding vs. comprehension gaps and adjust instructional emphasis accordingly? Does teacher judgment outperform algorithmic (assessment-driven) allocation?',
    'Observational studies of teacher decision-making; comparison of outcomes in teacher-judgment-based classrooms vs. standardized-protocol classrooms.',
    'If teacher judgment is unreliable, the balanced model''s delegation of time allocation to individual teachers will produce highly variable outcomes; some struggling readers will receive insufficient phonics or insufficient meaning engagement. If teacher judgment is reliable, the model''s central claim (professional teachers can implement both functions well) is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_judgment_reliability, empirical, 'Whether distributed teacher judgment produces better outcomes than centralized protocol.').

omega_variable(
    kernel_contest_empirically_resolvable,
    'Is the dispute between phonics-first and whole-language advocates ACTUALLY an empirical disagreement, or is it fundamentally an ideological/resource conflict dressed up as empirical dispute?',
    'Analysis of the grounds cited by each reading for rejecting the others. If disagreement persists despite shared empirical evidence, the dispute is not purely empirical.',
    'If the dispute is empirical, balanced literacy''s synthesis is the logical resolution and the constraint is coordination. If the dispute is ideological, balanced literacy is a politically-negotiated compromise that satisfies neither camp and extraction (vendor expansion, teacher workload, identity lock) is structural to the compromise itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_empirically_resolvable, conceptual, 'Whether the kernel contest is empirically resolvable or ideologically-rooted.').

omega_variable(
    struggling_reader_identity_fusion,
    'Does identity-lock on struggling readers (the internalization of ''I am a student who needs both intervention types'') help or harm their reading development and self-efficacy over time?',
    'Longitudinal studies tracking struggling readers'' self-concept, reading outcomes, and exit patterns as they age. Studies of struggling readers in phonics-only vs. whole-language-only vs. balanced classrooms comparing internalization of identity.',
    'If the identity fusion supports long-term reading growth and self-efficacy, it is a beneficial side effect of the constraint. If it creates learned helplessness or reading avoidance, the constraint''s extraction component harms its own beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(struggling_reader_identity_fusion, empirical, 'Whether internalized identity-lock on struggling readers is developmentally beneficial or harmful.').

omega_variable(
    alternative_kernel_reading_missing,
    'Is there a fifth reading of the reading-acquisition legitimacy kernel that is not represented among phonics-primacy, whole-language-primacy, structured-literacy, and balanced-integration? (E.g., ''reading is culturally-situated literacy practices, not a universal decoding+comprehension machine'' or ''reading development is irreducibly individual and no method works for everyone'')?',
    'Literature review of reading research and pedagogy traditions; interviews with reading educators across traditions.',
    'If an unrepresented reading exists, the kernel contest is incomplete and balanced literacy''s claim to be the empirical resolution is premature. If the four readings exhaust the space, balanced literacy is at least contesting the right boundaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_kernel_reading_missing, conceptual, 'Whether the kernel contest is exhaustively specified by the four named readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.18).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 5, 0.19).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 10, 0.2).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 15, 0.22).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 20, 0.23).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 5, 0.29).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 20, 0.29).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 25, 0.29).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__balanced_literacy_integration, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the reading_acquisition_legitimacy kernel. The other readings are phonics_decoding_primacy (code-first), whole_language_meaning_primacy (meaning-first), and structured_literacy_remediation (vulnerability-first). Each reading has a different epistemic structure, different beneficiary/victim configuration, and different ε value. They are linked because they contest a common kernel — which aspect of reading development (decoding, meaning, remedial structure, balanced coordination) is the legitimate foundation of instruction. The balanced reading claims empirical synthesis; the others dispute it. See cs_structure.reading_relations for the structural relationships between this reading and its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
