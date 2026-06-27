% ============================================================================
% CONSTRAINT STORY: learning_difficulty_substrate_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_learning_difficulty_substrate_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: learning_difficulty_substrate_flat_control
 *   human_readable: Learning Difficulty Substrate Attribution
 *   domain: educational_psychology/learning_theory/epistemology
 *
 * SUMMARY:
 *   Educational systems must explain why some students struggle with complex
 *   material while others do not. The dominant substrate attribution locates
 *   the barrier in student traits—cognitive capacity, prior preparation,
 *   effort, or inherent ability. This framing coordinates institutional
 *   response (assessment, tracking, remediation) but also extracts from
 *   learners by making them bear the cost and stigma of difficulty.
 *   Alternative attributions—locating barriers in material presentation,
 *   instructional design, or the match between learner background and content
 *   structure—exist but are systematically excluded from standard-setting
 *   processes. The constraint is claimed as tangled_rope: genuine
 *   coordination function (enables resource allocation and instructional
 *   response) combined with asymmetric extraction (shifts costs to learners,
 *   sustains remediation markets, requires active suppression of alternative
 *   framings).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(learning_difficulty_substrate_flat_control, 0.68).
domain_priors:suppression_score(learning_difficulty_substrate_flat_control, 0.71).
domain_priors:theater_ratio(learning_difficulty_substrate_flat_control, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(learning_difficulty_substrate_flat_control, extractiveness, 0.68).
narrative_ontology:constraint_metric(learning_difficulty_substrate_flat_control, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(learning_difficulty_substrate_flat_control, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(learning_difficulty_substrate_flat_control, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(learning_difficulty_substrate_flat_control, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(learning_difficulty_substrate_flat_control, tangled_rope).
narrative_ontology:human_readable(learning_difficulty_substrate_flat_control, "Learning Difficulty Substrate Attribution").
narrative_ontology:topic_domain(learning_difficulty_substrate_flat_control, "educational_psychology/learning_theory/epistemology").

domain_priors:requires_active_enforcement(learning_difficulty_substrate_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(learning_difficulty_substrate_flat_control, learning_difficulty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(learning_difficulty_substrate_flat_control, educational_institutions).
narrative_ontology:constraint_beneficiary(learning_difficulty_substrate_flat_control, assessment_industry).
narrative_ontology:constraint_beneficiary(learning_difficulty_substrate_flat_control, remediation_providers).
narrative_ontology:constraint_victim(learning_difficulty_substrate_flat_control, struggling_learners).
narrative_ontology:constraint_victim(learning_difficulty_substrate_flat_control, non_traditional_learners).
narrative_ontology:constraint_vindicates(learning_difficulty_substrate_flat_control, individual_deficit_model).
narrative_ontology:constraint_vindicates(learning_difficulty_substrate_flat_control, ability_as_fixed_trait).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define what counts as legitimate difficulty versus student deficit. They set curriculum pacing, assessment standards, and intervention thresholds. The substrate attribution they adopt determines resource allocation: if difficulty is in the material's presentation, they must redesign instruction; if difficulty is in the student, they can outsource remediation. Their institutional capacity and funding models create pressure toward student-deficit framings.
narrative_ontology:constraint_stakeholder(learning_difficulty_substrate_flat_control, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Produce diagnostic instruments that locate learning barriers. Revenue scales with the number of students identified as having deficits requiring measurement and tracking. A substrate framing that treats difficulty as residing in measurable student traits expands the addressable market; a framing that locates difficulty in instructional design or material structure would shift demand toward pedagogical consulting rather than student assessment.
narrative_ontology:constraint_stakeholder(learning_difficulty_substrate_flat_control, assessment_industry, beneficiary,
    organized, biographical, mobile, national).

% Deliver tutoring, intervention programs, and specialized instruction to students identified as struggling. Their business model depends on a steady flow of students diagnosed with learning deficits. A substrate attribution that frames difficulty as intrinsic to the learner sustains demand; an attribution that frames difficulty as a mismatch between material presentation and learner background would redirect resources toward universal design rather than individualized remediation.
narrative_ontology:constraint_stakeholder(learning_difficulty_substrate_flat_control, remediation_providers, beneficiary,
    organized, biographical, mobile, regional).

% Experience difficulty with complex material and are told the barrier is their own cognitive capacity, prior preparation, or effort. They internalize the deficit framing, which becomes part of their academic identity. The substrate attribution determines whether they receive instructional redesign or are tracked into remedial pathways that carry stigma and limit future opportunities. Exit means abandoning educational advancement entirely; the identity lock is the internalized belief that they lack the capacity to understand.
narrative_ontology:constraint_stakeholder(learning_difficulty_substrate_flat_control, struggling_learners, payer,
    powerless, biographical, identity_locked, local).

% Bring different background knowledge, learning styles, or life circumstances to the material. The dominant substrate attribution treats their difficulty as evidence of deficit rather than as signal that the material's presentation assumes a narrow range of prior experience. They pay through delayed progression, additional coursework, or exclusion from advanced tracks. Their constraint is less identity-locked than struggling learners because they often have external validation of competence from other domains.
narrative_ontology:constraint_stakeholder(learning_difficulty_substrate_flat_control, non_traditional_learners, payer,
    moderate, biographical, constrained, regional).

% Study the cognitive and social mechanisms of learning difficulty. They produce evidence about how material structure, prior knowledge activation, cognitive load, and instructional design interact to create barriers. Their research often shows difficulty is a function of the match between material presentation and learner background, not a fixed trait. They observe that the dominant substrate attribution in practice diverges from the empirical evidence base.
narrative_ontology:constraint_stakeholder(learning_difficulty_substrate_flat_control, learning_scientists, observer,
    organized, generational, analytical, global).

% Advocate for substrate attributions that locate difficulty in instructional design, material accessibility, and systemic barriers rather than student deficits. They are structurally excluded from standard-setting bodies and assessment design processes. Their framing would require institutions to invest in universal design and pedagogical innovation rather than student sorting and remediation, which threatens existing resource allocation patterns.
narrative_ontology:constraint_stakeholder(learning_difficulty_substrate_flat_control, progressive_educators, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for diagnosing why students struggle with complex material, enabling institutions to allocate resources, teachers to adjust instruction, and students to understand their own difficulties.
% TRANSFER_FUNCTION: Moves responsibility and cost from institutions (who would bear the expense of redesigning instruction and materials) to individual learners (who bear the stigma, time cost, and financial burden of remediation). Also moves revenue to assessment and remediation industries through the identification and treatment of student deficits.
% ABSENT_VOICES: Progressive educators and universal design advocates who would locate difficulty in material presentation and instructional structure are excluded from the standard-setting and assessment design processes that instantiate the substrate attribution. Students themselves have no voice in defining what counts as the 'real' barrier to their own understanding.
% DISAPPEARANCE_RATIONALE: If the dominant substrate attribution vanished, institutions would face immediate pressure to redesign curricula and instruction rather than sorting students into remedial tracks. The assessment industry would lose its primary market for deficit-identification instruments. Remediation providers would need to pivot toward instructional consulting. Students currently labeled as deficient would be freed from that identity frame, though they would still face the underlying challenge of accessing complex material.
% FOUNDING_PROBLEM: Early mass education needed a way to explain differential student outcomes in standardized instruction. Without a substrate attribution, the system had no principled basis for deciding whether to adjust instruction or to sort students.
% FOUNDING_PROBLEM_CORROBORATION: Educational institutions attest the problem is still live, citing persistent achievement gaps. Learning scientists and progressive educators attest the founding problem has been substantially solved by research showing difficulty is context-dependent and instructionally malleable, but the attribution persists because it serves institutional resource allocation needs. Independent cognitive science research from outside the benefiting parties supports the instructionally-malleable reading.
narrative_ontology:disappearance_verdict(learning_difficulty_substrate_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(learning_difficulty_substrate_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(learning_difficulty_substrate_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-27',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(learning_difficulty_substrate_flat_control, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(learning_difficulty_substrate_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(learning_difficulty_substrate_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(learning_difficulty_substrate_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the substrate attribution shifts institutional costs (instructional redesign) onto individual learners (remediation, stigma, opportunity cost) and creates markets for assessment and intervention services. Suppression is high (0.71) because maintaining the student-deficit framing requires actively excluding alternative attributions from curriculum design, teacher training, and assessment development. Theater ratio is moderate (0.42): the coordination function is real—institutions do need some framework for responding to differential outcomes—but a growing share of activity is performative adherence to deficit identification protocols that serve market and resource-allocation interests rather than learning. Accessibility collapse is moderate (0.48): alternative substrate attributions are available in the research literature and some practitioners adopt them, but they are structurally excluded from the institutional mechanisms that instantiate the dominant framing. Resistance is substantial (0.62): progressive educators, learning scientists, and some students actively contest the deficit model, but they lack the institutional power to change standard-setting processes.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat, the substrate attribution is a necessary coordination mechanism that enables rational resource allocation given budget constraints. From the struggling learner seat, the same attribution operates as a coercive identity assignment that forecloses educational opportunity. From the assessment industry seat, it is a legitimate market for diagnostic services. From the learning scientist seat, it is an empirically unsupported claim sustained by institutional interests. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational institutions are agenda-setters with constrained exit—they must adopt some substrate attribution and face institutional pressure toward deficit framings that minimize redesign costs. Assessment and remediation industries are beneficiaries with mobile exit—they profit from the deficit framing but could pivot to other markets. Struggling learners are powerless payers with identity-locked exit—they internalize the deficit attribution as part of their academic self-concept, making exit psychologically costly even when physically possible. Non-traditional learners are moderate-power payers with constrained exit—they face barriers but often have external validation that prevents full identity lock. Learning scientists are analytical observers. Progressive educators are excluded moderate-power actors whose alternative framing would redistribute costs back to institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate—explaining differential learning outcomes to enable institutional response—remains live. But the specific substrate attribution that locates barriers in student deficits has outlived its empirical justification. Learning science shows difficulty is largely a function of instructional design and prior knowledge activation, not fixed traits. The attribution persists because it serves institutional resource allocation and market interests, not because it accurately describes the learning process. This is a clear case of mandatrophy: the coordination function is real, but the specific mechanism has become extractive rent-seeking dressed in the language of educational necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_location_empirics,
    'Where does learning difficulty actually reside: in stable student traits, in the match between material presentation and learner background, or in instructional design choices?',
    'Systematic comparison of learning outcomes under different instructional designs holding student population constant, or tracking individual students across contexts with varying instructional approaches. If difficulty is trait-based, it should be stable across contexts; if it is instructional, it should vary with design.',
    'If difficulty is primarily instructional, the current substrate attribution is empirically false and the extraction it enables is unjustified. If difficulty is primarily trait-based, the coordination function is legitimate and extraction is the price of accurate sorting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_location_empirics, empirical, 'Whether learning difficulty is a stable trait or a context-dependent phenomenon.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity lock on struggling learners structural (they lack actual capacity) or internalized (they have been taught to believe they lack capacity)?',
    'Longitudinal studies tracking learners who exit deficit-framed environments and enter contexts with alternative substrate attributions. If performance improves substantially, the lock was internalized; if it remains stable, it was structural.',
    'If the lock is internalized, the measured suppression understates the constraint''s coercive force—learners carry the barrier with them after the external mechanism is removed. If structural, the suppression measure is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether the identity lock is cognitive or social.').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function (enabling institutional response to differential outcomes) be separated from the extraction function (shifting costs to learners and sustaining remediation markets)?',
    'Natural experiments from educational systems that adopt universal design frameworks: if they maintain effective resource allocation while eliminating deficit-based tracking, the functions are separable.',
    'If separable, the constraint is a clear tangled rope where extraction rides on genuine coordination. If inseparable, some of the measured extraction is the necessary cost of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination requires deficit attribution or can operate through alternative framings.').

omega_variable(
    theater_ratio_trajectory,
    'Is the rising theater ratio (from 0.22 to 0.42 over the interval) driven by genuine Goodhart drift (assessment protocols replacing learning goals) or by increasing visibility of pre-existing performative compliance?',
    'Historical analysis of assessment practice: if early-interval assessments were tightly coupled to instructional goals and later ones became decoupled, it is Goodhart drift. If the decoupling was always present but became more visible through documentation, it is visibility change.',
    'Goodhart drift would indicate the constraint is degrading toward piton (theater without function). Visibility change would indicate stable tangled-rope operation with better measurement over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_ratio_trajectory, empirical, 'Whether theater ratio increase reflects functional decay or measurement improvement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(learning_difficulty_substrate_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lear_tr_t0, learning_difficulty_substrate_flat_control, theater_ratio, 0, 0.22).
narrative_ontology:measurement(lear_tr_t8, learning_difficulty_substrate_flat_control, theater_ratio, 8, 0.26).
narrative_ontology:measurement(lear_tr_t16, learning_difficulty_substrate_flat_control, theater_ratio, 16, 0.31).
narrative_ontology:measurement(lear_tr_t24, learning_difficulty_substrate_flat_control, theater_ratio, 24, 0.35).
narrative_ontology:measurement(lear_tr_t32, learning_difficulty_substrate_flat_control, theater_ratio, 32, 0.39).
narrative_ontology:measurement(lear_tr_t40, learning_difficulty_substrate_flat_control, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(lear_be_t0, learning_difficulty_substrate_flat_control, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(lear_be_t8, learning_difficulty_substrate_flat_control, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(lear_be_t16, learning_difficulty_substrate_flat_control, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(lear_be_t24, learning_difficulty_substrate_flat_control, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(lear_be_t32, learning_difficulty_substrate_flat_control, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(lear_be_t40, learning_difficulty_substrate_flat_control, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lear_su_t0, learning_difficulty_substrate_flat_control, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(lear_su_t8, learning_difficulty_substrate_flat_control, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(lear_su_t16, learning_difficulty_substrate_flat_control, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(lear_su_t24, learning_difficulty_substrate_flat_control, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(lear_su_t32, learning_difficulty_substrate_flat_control, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(lear_su_t40, learning_difficulty_substrate_flat_control, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(learning_difficulty_substrate_flat_control, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
