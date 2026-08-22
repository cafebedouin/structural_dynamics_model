% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Whole-Language Reading Acquisition Pedagogy
 *   domain: educational/psychological
 *
 * SUMMARY:
 *   Whole-language reading theory holds that children learn to read through
 *   immersion in meaningful, authentic text rather than through explicit
 *   systematic phonics instruction. This reading of the reading-acquisition
 *   kernel has dominated teacher preparation and curriculum adoption in many
 *   Anglophone jurisdictions since the 1980s. It minimizes upfront
 *   instructional scaffolding and maximizes teacher autonomy, but produces a
 *   steeply asymmetric outcome distribution: children with rich literacy
 *   environments and strong implicit pattern recognition thrive, while
 *   struggling readersâparticularly those with dyslexia and other decoding
 *   disabilitiesâfail to acquire foundational skills and require intensive,
 *   expensive remediation. The constraint is actively enforced through
 *   teacher credentialing programs, textbook adoption criteria, and
 *   professional development infrastructures that marginalize explicit
 *   phonics alternatives. This constraint story represents the whole-language
 *   reading as one reading of the contested kernel; sibling readings
 *   (systematic phonics, balanced literacy) are modeled as separate
 *   constraints.
 *
 * KEY AGENTS:
 *   - literacy_education_faculty: Primary agenda-setter (institutional/identity_locked) â controls credentialing, research, and professional development
 *   - classroom_teachers: Primary beneficiary (moderate/constrained) â gains autonomy and reduced script burden
 *   - struggling_readers: Primary target (powerless/trapped) â bears the cost of implicit decoding failure
 *   - parents_of_struggling_readers: Secondary target (moderate/constrained) â pays for remediation and advocacy
 *   - reading_researchers: Analytical observer (organized/analytical) â sees the structural failure but is marginalized in curriculum decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.62).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.58).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole-Language Reading Acquisition Pedagogy").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational/psychological").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, 'cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4').
narrative_ontology:cs_kernel_codification('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', formalized).
narrative_ontology:cs_authority_grounding('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', expertise).
narrative_ontology:cs_interpretation_layer_present('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4').
narrative_ontology:cs_reading_relation('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', reading_acquisition_mechanism__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', foundational, decoding_emerges_implicitly).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly, holdable).
narrative_ontology:cs_axiom_grounding('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', decoding_emerges_implicitly, empirically_contingent).
narrative_ontology:cs_axiom('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', foundational, authentic_text_primacy).
narrative_ontology:cs_axiom_status(authentic_text_primacy, holdable).
narrative_ontology:cs_axiom_grounding('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', authentic_text_primacy, instrumental).
narrative_ontology:cs_reference_frame('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', authentic_literacy_immersion).
narrative_ontology:cs_drift_state('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', post_systematic_review_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cd42bf48-7ed5-41f5-9b2c-318fe2ccc3a4', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, literacy_education_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, implicit_learning_hypothesis).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, constructivist_pedagogy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls teacher preparation curricula, professional development content, peer-review editorial boards, and textbook adoption criteria in literacy. Their professional standing, publication records, and institutional authority are built on the whole-language framework; moving to explicit phonics would require discarding decades of accumulated professional capital.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, literacy_education_faculty, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive professional autonomy and reduced lesson-preparation burden by avoiding rigid, scripted phonics sequences. They implement district-mandated whole-language curricula, manage leveled classroom libraries, and assess students through holistic, meaning-based rubrics rather than discrete decoding metrics.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, classroom_teachers, beneficiary,
    moderate, biographical, constrained, national).

% Expected to acquire decoding implicitly through immersion in authentic text. When implicit pattern recognition fails, they are diagnosed with learning disabilities or blamed for lack of engagement or inadequate home environments rather than instructional deficiency. They suffer compounding academic failure and require intensive, expensive remediation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, national).

% Observe their children failing to acquire basic reading skills in whole-language classrooms. Must pay for private phonics tutoring, educational advocacy, or legal action to obtain appropriate instruction. Their concerns are frequently dismissed by schools citing whole-language orthodoxy about developmental appropriateness.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, national).

% Cognitive scientists and reading researchers who produce evidence that explicit systematic phonics is necessary for typical and at-risk readers. They are structurally marginalized in teacher credentialing and curriculum adoption processes despite strong empirical findings, and they operate from an analytical seat outside the institutional enforcement apparatus.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, reading_researchers, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a low-burden, teacher-autonomous approach to early literacy that avoids rigid instructional scripts and prioritizes student engagement with authentic literature, coordinating classrooms around meaning-making rather than skill sequencing.
% TRANSFER_FUNCTION: Moves the cost and responsibility of acquiring foundational decoding skills from the instructional system (which avoids explicit sequencing) to individual students (who must internalize patterns implicitly) and to families and public remediation systems (which bear the expense and labor of later intervention).
% ABSENT_VOICES: Cognitive scientists and explicit-phonics reading researchers are structurally excluded from curriculum adoption committees and teacher credentialing programs; parents of children with dyslexia and decoding disabilities are present in schools but excluded from curricular decision-making; special education advocates who predict failure under implicit approaches are sidelined in general-education policy discourse.
% DISAPPEARANCE_RATIONALE: If whole-language theory vanished overnight, classroom practice would shift toward systematic phonics and structured literacy, teacher preparation programs would rewrite methods courses, the textbook and leveled-reader markets would reorganize around decodable texts, and the incidence of decoding failure requiring special education and remediation would fall. The literacy education field would reorganize around different epistemic premises.
% FOUNDING_PROBLEM: Early reading instruction in the mid-twentieth century was perceived as overly rigid, decontextualized, and demotivating, producing mechanical decoding without comprehension or love of reading; a method was needed that preserved meaning-making and intrinsic motivation from the outset.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language advocates and literacy education faculty attest that rigid instruction remains a threat to student motivation. Cognitive scientists, reading researchers, and special education advocates outside the benefiting parties attest that the rigidity narrative was overstated and that systematic phonics produces both decoding accuracy and motivation through competence; the National Reading Panel and subsequent meta-analyses from outside the literacy education establishment support the shifted-function reading.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62) because the constraint shifts the cost of decoding failure from the instructional system to the least powerful students. Suppression is moderate (0.58) because phonics alternatives are structurally marginalized in teacher training and textbook markets but survive in research and some policy spaces. Theater ratio is moderate (0.42): the apparatus of leveled libraries, holistic assessment, and professional development workshops maintains the appearance of literacy instruction while avoiding the explicit teaching that would serve struggling readers. Accessibility collapse is substantial (0.65) for trapped students in mandated districts. Resistance is high (0.72) and rising from cognitive science and parent advocacy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (literacy education faculty) experiences the constraint as a legitimate expertise-based framework that preserves teacher professionalism and child engagement. The payer seats (struggling readers and their parents) experience the same structure as a failure to teach, where the system's avoidance of explicit instruction generates private costs and long-term harm. The engine computes this divergence from the structural asymmetry in power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The literacy education faculty and classroom teachers are structurally near the beneficiary end: they receive autonomy, status, and reduced instructional burden (low d). Struggling readers and parents are near the target end: they pay with literacy failure and remediation costs, and their exit is blocked by compulsory schooling and district mandates (high d). Reading researchers sit near neutral/analytical: they neither collect from nor pay into the constraint, but their analytical exit prevents identity lock.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdisengaged, demotivated readers in overly rigid programsâwas live in the 1970s and 1980s. It is now contested: while engagement remains important, evidence shows that explicit phonics instruction produces both competence and motivation. The persistence of whole-language infrastructure beyond the resolution of its founding problem risks mandatrophy (zombie status), but the constraint still performs a live coordination function for teachers (autonomy) and still commands institutional enforcement, so it reads as tangled rope rather than pure piton. If teacher autonomy were the only remaining function and the decoding narrative were purely theatrical, it would approach piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_acquisition_empirical_status,
    'Is decoding skill truly acquired implicitly through meaningful text exposure for typical learners, or does all learning require some explicit attention to grapheme-phoneme correspondence?',
    'Large-scale randomized controlled trials comparing implicit immersion versus explicit phonics, with decoding outcomes measured by standardized assessments.',
    'If implicit acquisition is unsupported for typical learners, the coordination story collapses and the constraint reclassifies toward snare; if supported for a subset, tangled rope remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_acquisition_empirical_status, empirical, 'Whether implicit decoding acquisition is empirically valid').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of phonics in teacher training programs structural (institutional gatekeeping by literacy faculty) or internalized (teachers believe explicit phonics is developmentally inappropriate)?',
    'Surveys of teacher beliefs correlated with program accreditation requirements; observation of whether phonics resistance persists after structural barriers (e.g., mandates) are removed.',
    'If internalized, effective suppression exceeds the structural measureâthe constraint persists even when institutional enforcement weakens, raising theater ratio and supporting piton dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of phonics alternatives').

omega_variable(
    founding_problem_contestation,
    'Was the founding problem of demotivating, rigid phonics instruction historically accurate, or a strawman that justified replacing one incomplete method with another?',
    'Historical curriculum analysis of pre-whole-language phonics programs versus contemporary systematic phonics to assess the rigidity claim.',
    'If the founding problem was overstated, the constraint''s original coordination justification is weakened and the current extraction dominates the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_contestation, conceptual, 'Historical accuracy of the founding problem narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(read_be_t32, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(read_su_t32, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reading_acquisition_mechanism kernel. It is structurally decomposed from its sibling readings because the label 'reading acquisition' conflates three distinct empirical claims with different epsilon values and different beneficiary/victim structures. Whole-language reading and phonics reading cannot be coherently evaluated as the same constraint viewed from different angles; their core premises contradict one another at the foundational level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
