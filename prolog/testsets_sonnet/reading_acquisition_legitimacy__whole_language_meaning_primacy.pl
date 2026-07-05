% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__whole_language_meaning_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole Language Meaning-Primacy Reading Instruction Doctrine
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the reading_acquisition_legitimacy
 *   kernel: the whole-language meaning-primacy claim that reading is
 *   fundamentally meaning-making, and legitimate instruction consists of
 *   immersing children in authentic literature and letting decoding emerge as
 *   a byproduct of that immersion, with individualized support (running
 *   records, guided reading) for students who do not spontaneously acquire
 *   the code. This is distinct from — not a measurement variant of — the
 *   phonics_decoding_primacy reading (reading is decoding; legitimate
 *   instruction makes the alphabetic code explicit), the
 *   balanced_literacy_integration reading (both are required; legitimate
 *   instruction balances them), and the structured_literacy_remediation
 *   reading (instruction must be designed for the most vulnerable learner
 *   first, following explicit cumulative diagnostic principles). Each of
 *   those is a separate constraint story with its own epsilon; this file's
 *   epsilon (0.58, rising over the measured interval) describes only the
 *   meaning-primacy claim's actual operation — the accumulating cost borne by
 *   learners who do not induct the code from exposure alone.
 *
 * KEY AGENTS:
 *   - whole_language_teacher_educators: institutional agenda-setter — trains teachers, controls what counts as legitimate pedagogy in schools of education
 *   - basal_and_leveled_text_publishers: organized beneficiary — sells materials engineered for the meaning-primacy classroom
 *   - literacy_consultants_and_coaches: organized beneficiary/agenda-setter — profits from coaching contracts built on the framework
 *   - struggling_decoders, children_from_low_print_exposure_homes, english_language_learners, students_with_dyslexia: powerless, trapped payers — bear the cost of a method that assumes background scaffolding they lack
 *   - cognitive_science_reading_researchers: analytical observer — external empirical corroboration outside the benefiting institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.52).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Meaning-Primacy Reading Instruction Doctrine").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '04e4ec49-9b4d-42d9-ad01-3c94a15c280a').
narrative_ontology:cs_kernel_codification('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', distributed).
narrative_ontology:cs_authority_grounding('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', practice).
narrative_ontology:cs_interpretation_layer_present('04e4ec49-9b4d-42d9-ad01-3c94a15c280a').
narrative_ontology:cs_reading_relation('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', foundational, decoding_emerges_from_meaningful_engagement).
narrative_ontology:cs_axiom_status(decoding_emerges_from_meaningful_engagement, holdable).
narrative_ontology:cs_axiom_grounding('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', decoding_emerges_from_meaningful_engagement, empirically_contingent).
narrative_ontology:cs_axiom('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', secondary, authentic_literature_from_day_one_is_pedagogically_necessary).
narrative_ontology:cs_axiom_status(authentic_literature_from_day_one_is_pedagogically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', authentic_literature_from_day_one_is_pedagogically_necessary, instrumental).
narrative_ontology:cs_reference_frame('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', meaning_centered_progressive_pedagogy).
narrative_ontology:cs_drift_state('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', post_science_of_reading_legislative_wave, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('04e4ec49-9b4d-42d9-ad01-3c94a15c280a', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_teacher_educators).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, basal_and_leveled_text_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_consultants_and_coaches).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, fluent_readers_from_literacy_rich_homes).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_decoders).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_from_low_print_exposure_homes).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, english_language_learners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, students_with_dyslexia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design teacher-preparation curricula around meaning-first pedagogy, train new teachers in guided reading and running records rather than systematic phonics, and control what counts as legitimate literacy instruction in schools of education. Their professional identity, publications, and institutional standing are built on the meaning-primacy framework; changing it would devalue decades of training materials and career output.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_teacher_educators, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell leveled-reader sets, predictable-text series, and guided-reading kits engineered for meaning-primacy classrooms. Revenue depends on continued adoption of the pedagogy; they can pivot product lines if the market shifts, but currently profit directly from the constraint's persistence.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, basal_and_leveled_text_publishers, beneficiary,
    organized, biographical, mobile, national).

% Sell professional development, coaching contracts, and running-records assessment training to districts committed to the framework. They both administer the model in classrooms via coaching and profit from its continued district-level adoption.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_consultants_and_coaches, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_consultants_and_coaches, agenda_setter).

% Enter school with substantial prior print exposure, oral vocabulary, and often informal decoding skill from home reading. For these children immersion in authentic texts largely works — they infer letter-sound patterns from exposure because they already have the scaffolding the method assumes everyone has.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, fluent_readers_from_literacy_rich_homes, beneficiary,
    moderate, biographical, mobile, local).

% Do not spontaneously induce the alphabetic code from context and pictures; without explicit, systematic phonics they plateau, develop compensatory guessing strategies from pictures and initial letters, and fall progressively further behind as text complexity rises past the point where guessing works. They cannot select their own classroom or curriculum.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_decoders, payer,
    powerless, biographical, trapped, local).

% Arrive at school with less oral vocabulary and print exposure than peers; the method's core assumption — that authentic immersion alone will produce decoding — depends on background knowledge and vocabulary these children have had less opportunity to build, widening rather than closing the gap the school is meant to close.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_from_low_print_exposure_homes, payer,
    powerless, biographical, trapped, local).

% Are asked to infer an unfamiliar phonological and orthographic system from meaning context in a language they are simultaneously still acquiring, compounding the demand the method makes on background knowledge they do not yet have in English.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, english_language_learners, payer,
    powerless, biographical, trapped, local).

% Have a specific neurological difficulty mapping sound to print that requires explicit, systematic, cumulative instruction to overcome; meaning-primacy immersion without that scaffolding leaves the core deficit untreated, and their difficulty is frequently misread as a motivation or comprehension problem rather than a decoding one.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, students_with_dyslexia, payer,
    powerless, biographical, trapped, local).

% Observe their children failing to progress and often seek outside tutoring or advocacy, but have little influence over district curriculum adoption, which is set by educators and administrators; their objections are frequently reframed by the system as parental anxiety rather than evidence of pedagogical failure.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, excluded,
    powerless, biographical, constrained, local).

% Conduct and review the empirical literature on word recognition, phonological awareness, and the simple view of reading. Their converging findings on the necessity of explicit code instruction are frequently cited by critics of the meaning-primacy approach but sit outside the schools-of-education institutions that set teacher training curricula.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, cognitive_science_reading_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a classroom-wide instructional approach: teachers, curriculum materials, and assessment (running records, guided reading levels) are aligned around a shared theory that reading develops through meaningful engagement with real texts, giving a professional community a coherent, teachable, assessable framework rather than fragmented individual practice.
% TRANSFER_FUNCTION: Moves instructional time, curricular material adoption dollars, and children's early literacy development trajectory away from explicit code instruction and toward text immersion and meaning-strategy practice; the cost of the resulting decoding gap is transferred onto struggling readers and their families as later remediation burden, while training, materials, and coaching revenue flow to educators, publishers, and consultants who maintain the framework.
% ABSENT_VOICES: Cognitive science reading researchers whose work on phonological processing and the simple view of reading is well-established are largely absent from schools-of-education curricula; parents of struggling readers who observe the failure firsthand have no institutional standing to change district adoption; adult illiterate and semi-literate former students who exited the system without functional decoding are not present in any policy conversation at all.
% DISAPPEARANCE_RATIONALE: If meaning-primacy instruction disappeared overnight, teacher preparation programs would need to retool their curricula, leveled-text and basal publishers would lose a core product line, literacy coaching contracts built around running records and guided reading would need restructuring, and — most consequentially — struggling decoders, ELLs, and dyslexic students would receive systematic phonics instruction that the current framework does not centrally provide, materially changing outcomes for a large minority of students.
% FOUNDING_PROBLEM: Mid-20th-century phonics-first instruction had become mechanical and de-contextualized — children could sound out words without understanding or enjoying what they read; whole language arose to reconnect decoding practice to genuine comprehension, motivation, and love of reading, reacting against drill-heavy, meaning-starved instruction.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language teacher educators and consultants attest the founding problem — meaning-starved, mechanical drill — remains live and that immersion approaches solve it. Cognitive science reading researchers, including large-scale meta-analyses (e.g. National Reading Panel and subsequent replications) conducted substantially outside schools-of-education institutions, attest that the original problem was real but the proposed solution empirically fails a substantial minority of learners who require explicit code instruction; this corroboration comes from outside the beneficiary set and is the basis for the contested status.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.58 over the interval as the empirical case against unassisted code-emergence (phonological awareness research, the simple view of reading, dyslexia neuroscience) accumulated externally while the framework's institutional position in teacher preparation hardened rather than adapted — later cohorts of struggling readers pay a cost the method's own defenders increasingly cannot attribute to insufficient authentic-text exposure. Suppression (0.52 final) reflects active resistance within schools of education to systematic phonics adoption, not mere inertia — programs that attempted to introduce explicit code instruction alongside meaning-primacy immersion frequently met institutional pushback from faculty invested in the framework. Theater ratio (0.46) captures the growing share of running-records and guided-reading assessment activity that functions to document struggle within the paradigm rather than to change the paradigm — increasingly elaborate diagnostic machinery paired with a persistently non-explicit instructional core.
 *
 * DIRECTIONALITY LOGIC:
 *   Teacher educators, consultants, and publishers sit near the beneficiary end: they set or profit from the framework and can pivot professionally if it changes (arbitrage/mobile exit). Fluent readers from literacy-rich homes are also beneficiaries in effect — the method's core assumption (that immersion suffices) happens to be true for them because their homes supplied the missing scaffolding, so the constraint costs them little while still crediting the method with their success. Struggling decoders, low-print-exposure children, ELLs, and dyslexic students are targets: trapped exit (they cannot select classrooms or curricula), powerless, and the ones for whom the method's central assumption is false. This is not a symmetric distributional story — the same instructional theory is validated by the population it fits and falsified by the population it doesn't, and the latter has no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mechanical, meaning-starved phonics drill — was real and the reaction against it was a legitimate corrective function; that history is why this is authored as tangled_rope rather than snare. The coordination function (a shared, teachable, assessable instructional theory) persists and is not in itself extractive. But the founding problem's status is now contested rather than settled: cognitive science evidence developed largely outside the beneficiary institutions indicates that a substantial minority of learners require explicit code instruction the framework does not centrally supply, and the framework has hardened defensively rather than absorbing that evidence. Classifying this as tangled_rope rather than snare preserves the genuine coordination history while still naming the asymmetric, enforced cost borne by struggling decoders — exactly the mislabeling mandatrophy analysis exists to prevent in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaning_primacy_kernel_reading_ambiguity,
    'Is the whole-language claim that decoding emerges naturally from authentic-text immersion a genuinely distinct empirical hypothesis from the phonics-primacy and balanced-literacy readings, or is it better understood as a contested interpretive gloss on a single underlying kernel (what does ''legitimate reading instruction'' require)?',
    'Track whether meta-analytic reading research treats these as competing causal hypotheses about acquisition mechanism (supporting the distinct-constraint reading, as this file assumes) or as points on a continuous instructional-emphasis spectrum (supporting a single-kernel-with-degree reading).',
    'If the readings are genuinely distinct causal claims, each deserves its own epsilon and stakeholder structure (as authored here). If they collapse to a spectrum, the four-story decomposition may be over-fragmented and a single constraint with an internal emphasis parameter would be more accurate — though this would violate the epsilon-invariance principle if emphasis produces different epsilon values, which is exactly the evidence for decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaning_primacy_kernel_reading_ambiguity, conceptual, 'Whether the kernel''s four readings are structurally distinct constraints or degrees of one constraint.').

omega_variable(
    beneficiary_capture_vs_genuine_belief,
    'Do whole-language teacher educators and consultants maintain the framework primarily because they genuinely believe the empirical case for it, or because their professional identity and institutional position depend on it?',
    'Compare adoption rates and revision speed in teacher-education programs against the pace and weight of contrary empirical evidence (e.g., the 2019-2023 ''science of reading'' legislative wave); rapid revision under evidentiary pressure would support genuine-belief-updating, persistent resistance despite pressure would support identity/institutional capture.',
    'If capture dominates, the tangled_rope classification is conservative and the constraint trends toward snare as the founding-problem-status gap widens; if genuine belief dominates and evidence is more contested than presented here, the constraint may be closer to rope with an ongoing, unresolved scientific dispute rather than an enforced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_belief, empirical, 'Whether persistence reflects institutional self-interest or an unresolved good-faith scientific dispute.').

omega_variable(
    socioeconomic_confound_in_victim_identification,
    'How much of the struggling-decoder outcome gap is attributable to the instructional method itself versus confounding socioeconomic and home-literacy factors that would produce similar gaps under any instructional method?',
    'Randomized or quasi-experimental comparison of code-emphasis versus meaning-emphasis instruction within matched socioeconomic strata; the existing research base (e.g. National Reading Panel, subsequent RCTs) partially addresses this but full confound control is contested.',
    'If most of the gap is confound rather than instructional-method effect, extractiveness attributed to this constraint specifically should be lower and more diffusely shared with broader socioeconomic constraints; if the method effect is robust across strata (as most current meta-analytic evidence suggests), the authored extractiveness and victim declarations stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(socioeconomic_confound_in_victim_identification, empirical, 'Whether the outcome gap is caused by the method or by confounded background factors the method cannot control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0, 0.2).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 8, 0.26).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 16, 0.32).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 24, 0.38).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 32, 0.43).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 40, 0.46).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(read_be_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(read_be_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(read_su_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 32, 0.49).
narrative_ontology:measurement(read_su_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This file is one of four sibling constraints decomposing the natural-language concept 'legitimate reading instruction' per the epsilon-invariance principle. whole_language_meaning_primacy (this file) claims tangled_rope with rising extractiveness (0.35->0.58) driven by an accumulating gap between institutional practice and external cognitive-science evidence. phonics_decoding_primacy is expected to show low extractiveness and strong empirical corroboration (closer to rope or mountain-adjacent, given converging cognitive science support). balanced_literacy_integration sits between, coordinating both camps but subject to its own enforcement and resource-allocation dynamics. structured_literacy_remediation is a diagnostic-first reading likely closest to rope, prioritizing the most vulnerable learners explicitly. All four are readings of the same reading_acquisition_legitimacy kernel and must remain linked via affects_constraints; none should be treated as a measurement variant of another — each has been authored with its own stable epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
