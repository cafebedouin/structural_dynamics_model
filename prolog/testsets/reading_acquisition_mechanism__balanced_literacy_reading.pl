% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Acquisition Framework
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Balanced literacy is an institutional framework adopted in most U.S.
 *   states and many English-speaking countries that claims reading
 *   acquisition requires both explicit phonics instruction AND authentic
 *   literature engagement integrated into classroom practice. The framework
 *   emerged in the 1990s-2000s as a compromise between whole-language and
 *   phonics-first advocates, institutionalized through state standards,
 *   teacher training programs, and educational publishing. It is ONE READING
 *   of the contested kernel 'reading acquisition mechanism' — one of three
 *   structurally distinct claims about how children learn to read. This
 *   reading assumes both phonics and literature engagement are necessary and
 *   should be integrated. However, the framework exhibits substantial
 *   implementation fidelity collapse: in under-resourced schools, phonics
 *   components often remain performative while whole-language practice
 *   dominates, suggesting the framework functions partly as institutional
 *   cover for preferences it claims to balance.
 *
 * KEY AGENTS:
 *   - reading_education_researchers_balanced_camp (institutional power, arbitrage exit): define the framework, maintain it in policy and practice, benefit from its continued adoption
 *   - teacher_training_institutions (organized power, constrained exit): commit to balanced literacy curricula, face costs if frameworks shift
 *   - educational_publishing_conglomerates (powerful, arbitrage exit): capture market share through aligned materials, maintain portfolio flexibility
 *   - classroom_teachers_resource_constrained (moderate power, identity-locked exit): implement mandated practice with insufficient resources, cannot easily exit teaching profession
 *   - struggling_early_readers & students_from_print_poor_backgrounds (powerless, trapped exit): subject to framework without voice in its design, experience insufficient phonics intensity
 *   - state_education_departments (institutional power, analytical exit): enforce framework through standards and adoption, maintain institutional distance from theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.62).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Reading Acquisition Framework").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '6955b082-0d4c-41a1-8316-79289fcb3bc7').
narrative_ontology:cs_kernel_codification('6955b082-0d4c-41a1-8316-79289fcb3bc7', distributed).
narrative_ontology:cs_authority_grounding('6955b082-0d4c-41a1-8316-79289fcb3bc7', expertise).
narrative_ontology:cs_reading_relation('6955b082-0d4c-41a1-8316-79289fcb3bc7', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('6955b082-0d4c-41a1-8316-79289fcb3bc7', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('6955b082-0d4c-41a1-8316-79289fcb3bc7', foundational, integration_necessity).
narrative_ontology:cs_axiom_status(integration_necessity, holdable).
narrative_ontology:cs_axiom_grounding('6955b082-0d4c-41a1-8316-79289fcb3bc7', integration_necessity, empirically_contingent).
narrative_ontology:cs_axiom('6955b082-0d4c-41a1-8316-79289fcb3bc7', foundational, methodological_pluralism).
narrative_ontology:cs_axiom_status(methodological_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('6955b082-0d4c-41a1-8316-79289fcb3bc7', methodological_pluralism, instrumental).
narrative_ontology:cs_reference_frame('6955b082-0d4c-41a1-8316-79289fcb3bc7', integrated_reading_instruction_framework).
narrative_ontology:cs_drift_state('6955b082-0d4c-41a1-8316-79289fcb3bc7', contemporary_reading_science_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6955b082-0d4c-41a1-8316-79289fcb3bc7', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, reading_education_researchers_balanced_camp).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_institutions).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, educational_publishing_conglomerates).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_early_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, students_from_print_poor_backgrounds).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers_resource_constrained).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.58 at interval end) because the framework benefits institutional stakeholders (researchers, publishers, teacher trainers) while extracting implementation labor and instructional time from teachers and students. The framework claims to avoid ideological commitment while systematically privileging institutional stability over student outcomes — this is the asymmetric extraction. Suppression is high (0.62) because the framework suppresses competing positions (phonics-first and whole-language advocates) through institutional channels (state standards, adoption committees, teacher training curricula) without engaging their empirical arguments directly. Theater is substantial and rising (0.48 at interval end): phonics components are often performed (scripted lesson sequences, compliance artifacts) rather than delivered with sufficient intensity. The time series shows extraction and theater rising from 2000–2015 as the framework became institutionalized, then plateauing as it achieved dominance — the plateau indicates the constraint has reached steady-state extraction rather than continuing to expand. Suppression follows the same pattern: initial rise as institutional machinery was built, then stable maintenance at high level.
 *
 * PERSPECTIVAL GAP:
 *   From the researcher/publisher perspective (agenda-setter seat), balanced literacy is a genuine coordination solution to a real problem: the reading science evidence genuinely does show support for both phonics and meaning-centered approaches, and the framework integrates them. From the struggling reader perspective (payer seat), the framework is an extraction mechanism: their need for intensive decoding support is subordinated to institutional consensus building, and they receive neither component with sufficient intensity. From the teacher perspective (constrained-payer seat), the framework operates as a compliance mandate that leaves them responsible for implementing integration they lack time and resources to deliver — they experience it as extraction of their professional judgment and labor. From the state department perspective (agenda-setter seat), the framework allows institutional avoidance of commitment: it satisfies various constituencies without forcing a choice. The engine computes these divergent types from the structural data; the authored claim does not adjudicate which perspective is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary seats (researchers, publishers, teacher trainers) experience low directionality d (near 0.0): they benefit directly from the framework's adoption and have arbitrage exit options (can move to other frameworks if needed, maintain peer networks). The payer seats (teachers and students) experience high directionality d (near 1.0 for trapped students, 0.7 for constrained teachers): they bear the extraction (insufficient resources, compressed instructional time, misalignment between framework rhetoric and classroom reality) and have limited exit. The excluded seats (phonics advocates and whole-language advocates) have high d not because they pay directly but because the framework's enforcement explicitly suppresses their positions through institutional channels — they experience targeting of their legitimacy. Directionality overrides are unnecessary here; the structural derivation from beneficiary/victim declarations and exit options produces the right pattern.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy — the condition where a constraint's original mandate has become obsolete but the constraint persists — is a risk for balanced literacy if the founding problem (reading science evidence for both phonics and whole-language) is empirically resolved in favor of phonics-first frameworks. Recent meta-analyses and neuroscience evidence suggest the phonics component of reading science is substantially stronger than the whole-language component, which would mean the original compromise problem (equally valid competing claims) no longer exists as stated. If this is true, balanced literacy persists as institutional compromise even though the empirical problem it solves has shifted. However, this remains contested: some researchers defend whole-language components on grounds of motivation, comprehension, and engagement. Declaring mandatrophy here would require consensus that reading science has resolved the dispute in favor of phonics, which does not exist. The constraint is best classified as tangled-rope (genuine coordination function + asymmetric extraction) rather than piton (atrophied function + pure performance) because the coordination function (integrating phonics and literature) remains defensible even if implementation has degraded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_collapse,
    'Does balanced literacy persist as a genuine integrated practice, or does it systematically collapse into whole-language practice in under-resourced schools, with phonics components remaining performative?',
    'Classroom observation studies documenting time allocation to explicit phonics vs. authentic reading in schools claiming balanced literacy adoption; analysis of teacher practice by resource level.',
    'If collapse occurs systematically, the constraint functions as an institutional disguise for whole-language practice, shifting classification toward snare. The framework extracts the legitimacy of phonics advocacy while delivering whole-language instruction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_fidelity_collapse, empirical, 'Whether balanced literacy maintains integration fidelity or collapses to one component in practice.').

omega_variable(
    reading_science_foreclosure,
    'Does the current neuroscience and cognitive science evidence on reading acquisition structurally preclude the whole-language component, or do both components have defensible empirical grounding?',
    'Meta-analytic synthesis of reading science literature by systematic review teams outside the balanced literacy establishment; brain imaging studies on decoding automaticity.',
    'If reading science evidence forecloses whole-language instruction as ineffective, balanced literacy functions as a false compromise that suppresses phonics-first approaches in the name of integration. If both components remain empirically defensible, the framework is a genuine coordination solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_science_foreclosure, empirical, 'Whether reading science evidence supports or forecloses the whole-language component of balanced literacy.').

omega_variable(
    phonics_intensity_sufficiency,
    'What level of explicit phonics instruction is necessary for struggling readers and students from print-poor backgrounds to achieve grade-level decoding? Does balanced literacy as typically implemented meet that threshold?',
    'Intervention studies with randomized allocation to balanced literacy vs. intensive phonics; longitudinal tracking of reading outcomes by student demographic group and baseline decoding level.',
    'If intensive phonics is necessary for some populations and balanced literacy delivers insufficient phonics intensity, the framework extracts from those most vulnerable. If balanced intensity is sufficient across populations, the framework is equitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_intensity_sufficiency, empirical, 'Whether balanced literacy delivers sufficient phonics instruction for all populations.').

omega_variable(
    institutional_vs_empirical_grounding,
    'Is balanced literacy grounded primarily in empirical reading science findings, or has it become an institutional consensus position that benefits publishing and teacher training regardless of empirical support?',
    'Citation analysis of balanced literacy literature: how often does it cite vs. dismiss phonics-first and whole-language evidence? Historical timeline analysis of framework adoption relative to research publication.',
    'High institutional grounding relative to empirical grounding would indicate the framework functions partly as an extraction mechanism. Tight empirical grounding would support its legitimacy as a coordination device.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_empirical_grounding, conceptual, 'The epistemic basis for balanced literacy''s continued adoption.').

omega_variable(
    kernel_reading_ambiguity,
    'Does balanced literacy represent a genuine third position distinct from both phonics-first and whole-language, or is it a political compromise that suppresses both positions in the name of avoiding ideology?',
    'Qualitative analysis of how researchers, teachers, and administrators describe balanced literacy: do they claim it as an integrated framework, or as a tool for avoiding disputes? Examine how phonics-first and whole-language advocates perceive balanced literacy.',
    'If balanced literacy is perceived as a genuine integration, it coexists with its siblings. If perceived as a suppression mechanism, it forecloses or influences its siblings structurally rather than empirically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'The ontological status of balanced literacy as a reading of the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__balanced_literacy_reading, 0.18).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__whole_language_reading).

% DUAL FORMULATION NOTE:
% Balanced literacy is one reading of the contested kernel 'reading_acquisition_mechanism'. The phonics_reading and whole_language_reading constraints are sibling readings of the same kernel. All three readings share the empirical domain (reading science, child literacy development) but instantiate different structural constraints because they make different claims about what reading acquisition requires. Decomposition is necessary per epsilon-invariance: phonics_reading has a different epsilon (high extraction from whole-language advocates who dispute its claims, low extraction if it achieves evidence consensus), whole_language_reading has a different epsilon (lower extraction now as evidence moved against it, higher extraction historically), and balanced_literacy_reading has the epsilon authored here (moderate extraction disguised as institutional compromise). The epsilon difference is not observer-relative — it flows from different claims about what the kernel IS. Each reading gets its own constraint story with its own beneficiary/victim structure, its own directionality pattern, and its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
