% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   This story instantiates the whole_language_meaning_primacy reading of the
 *   reading_acquisition_legitimacy kernel: the claim that reading is
 *   fundamentally meaning-making, and that legitimate instruction immerses
 *   children in authentic literature from the start, trusting decoding skills
 *   to emerge naturally through exposure rather than requiring systematic,
 *   explicit teaching. The constraint governs which instructional practices,
 *   teacher-training programs, and curriculum materials count as legitimate
 *   in a school system that has adopted this framework. As enforced through
 *   teacher licensure requirements, school-of-education coursework, and
 *   district curriculum adoption, it functions as coordination (a coherent,
 *   motivating theory of literacy instruction) layered with extraction
 *   (systematic underservice of children whose reading acquisition depends on
 *   explicit phonemic instruction the method does not provide).
 *
 * KEY AGENTS:
 *   - credentialed_literacy_faculty: institutional agenda-setter and secondary beneficiary — sets what counts as legitimate pedagogy through teacher preparation
 *   - whole_language_curriculum_publishers: organized beneficiary — sells materials aligned with the paradigm
 *   - struggling_decoders / dyslexic_students / english_language_learners / low_income_students_without_home_text_exposure: powerless payers — bear the cost of unremediated decoding failure
 *   - cognitive_science_reading_researchers: excluded analytical voice — produces contrary evidence largely absent from teacher training
 *   - school_district_administrators: institutional observer/agenda-setter caught between institutional inertia and external reform pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.52).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Meaning-Primacy Reading Instruction Doctrine").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '193b4415-a5f2-40fb-b766-4a4f73cd6c58').
narrative_ontology:cs_kernel_codification('193b4415-a5f2-40fb-b766-4a4f73cd6c58', distributed).
narrative_ontology:cs_authority_grounding('193b4415-a5f2-40fb-b766-4a4f73cd6c58', practice).
narrative_ontology:cs_interpretation_layer_present('193b4415-a5f2-40fb-b766-4a4f73cd6c58').
narrative_ontology:cs_reading_relation('193b4415-a5f2-40fb-b766-4a4f73cd6c58', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('193b4415-a5f2-40fb-b766-4a4f73cd6c58', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_reading_relation('193b4415-a5f2-40fb-b766-4a4f73cd6c58', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('193b4415-a5f2-40fb-b766-4a4f73cd6c58', foundational, decoding_emerges_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(decoding_emerges_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('193b4415-a5f2-40fb-b766-4a4f73cd6c58', decoding_emerges_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('193b4415-a5f2-40fb-b766-4a4f73cd6c58', secondary, authentic_text_immersion_is_necessary_and_sufficient_entry_point).
narrative_ontology:cs_axiom_status(authentic_text_immersion_is_necessary_and_sufficient_entry_point, holdable).
narrative_ontology:cs_axiom_grounding('193b4415-a5f2-40fb-b766-4a4f73cd6c58', authentic_text_immersion_is_necessary_and_sufficient_entry_point, instrumental).
narrative_ontology:cs_reference_frame('193b4415-a5f2-40fb-b766-4a4f73cd6c58', whole_language_founding_consensus).
narrative_ontology:cs_drift_state('193b4415-a5f2-40fb-b766-4a4f73cd6c58', post_national_reading_panel_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('193b4415-a5f2-40fb-b766-4a4f73cd6c58', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, credentialed_literacy_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, fluent_readers_with_home_literacy_support).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_decoders).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, english_language_learners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, low_income_students_without_home_text_exposure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, meaning_construction_as_primary_reading_process).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, natural_language_acquisition_analogy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains generations of teachers in schools of education using meaning-primacy frameworks, sets licensure and accreditation expectations, and authors the textbooks and coursework that define what counts as legitimate reading pedagogy. Careers, tenure cases, and professional identity are built on the theoretical framework; abandoning it would mean repudiating decades of published scholarship and teacher preparation curricula.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, credentialed_literacy_faculty, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, credentialed_literacy_faculty, beneficiary).

% Produce and sell leveled-text libraries, running-records assessment kits, and guided-reading materials aligned with meaning-primacy instruction. Revenue depends on continued district adoption of this instructional model over systematic phonics programs; can pivot product lines if the market shifts, but current catalogs and sales relationships are built on the paradigm.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Implement the approach because it is what they were trained in and what their district mandates; many report discomfort when students fail to acquire decoding skills but lack alternative training or materials. Their professional evaluations and instructional autonomy are bound to the method their preparation program endorsed; deviating risks conflict with instructional coaches and administrators.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, beneficiary).

% Children without strong pattern-recognition or contextual-guessing skills fail to independently derive the alphabetic code from exposure alone and fall progressively behind grade-level reading benchmarks. They have no say in instructional method and cannot exit the classroom; remediation, if it comes, arrives only after measurable failure accumulates across school years.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_decoders, payer,
    powerless, biographical, trapped, local).

% Students with phonological processing differences are structurally the worst-served population under meaning-primacy instruction, since the approach withholds the explicit, systematic phonemic instruction this population specifically requires to acquire decoding. Diagnosis is frequently delayed because early reading difficulty is attributed to normal developmental variation rather than a mismatch between instructional method and neurocognitive need.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Children without extensive home exposure to print, shared reading, or a large vocabulary of memorized sight patterns lack the incidental scaffolding the meaning-primacy model implicitly assumes. The achievement gap between these students and their more literacy-resourced peers widens because the method's success depends on background exposure it does not itself provide.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, low_income_students_without_home_text_exposure, payer,
    powerless, generational, trapped, regional).

% Students still acquiring English oral vocabulary cannot reliably use context and meaning cues to guess unfamiliar words, undermining the core strategy the method teaches in place of decoding. They fall behind on both language acquisition and literacy simultaneously with no alternative instructional track offered.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, english_language_learners, payer,
    powerless, biographical, trapped, regional).

% Children who arrive at school already partially literate, with rich home vocabulary and print exposure, tend to succeed under the meaning-primacy model regardless of instructional method because they can bootstrap decoding from context and memorized whole-word recognition. Their success is often cited as evidence the method works generally.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, fluent_readers_with_home_literacy_support, beneficiary,
    moderate, biographical, mobile, local).

% Produce decades of converging experimental and neuroimaging evidence (the 'science of reading' literature) showing that skilled reading requires explicit phonemic decoding and that meaning-primacy instruction underperforms for a large share of learners. Their findings circulate in academic journals but have limited direct influence over teacher-preparation curricula, which remain governed by schools of education with separate institutional lineages.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, cognitive_science_reading_researchers, excluded,
    analytical, generational, analytical, global).

% Select curricula, allocate professional development budgets, and respond to standardized test outcomes and parent advocacy pressure. Increasingly caught between long-standing relationships with faculty-trained teaching staff and mounting external pressure (legislative mandates, parent literacy advocacy groups) to adopt systematic phonics-aligned curricula.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, school_district_administrators, observer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, school_district_administrators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, credentialed_literacy_faculty).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom instruction around a coherent theory of reading as an extension of natural oral language acquisition, using rich authentic texts to sustain motivation and comprehension focus rather than isolated skill drills, and aligning teacher training, curriculum materials, and assessment (running records) around a single pedagogical philosophy.
% TRANSFER_FUNCTION: Moves instructional authority and curriculum-adoption resources toward literacy faculty and whole-language-aligned publishers, and moves the cost of undiagnosed or unremediated decoding failure onto individual students — disproportionately those without offsetting home literacy resources or with neurocognitive processing differences — who must later seek remedial intervention, often after years of accumulated reading difficulty.
% ABSENT_VOICES: Cognitive science and reading-science researchers whose experimental findings on phonemic awareness and decoding are largely absent from teacher-preparation coursework; parents of dyslexic children who discover the mismatch only after their child has fallen substantially behind; the students themselves, who have no voice in instructional method selection at any point in the process.
% DISAPPEARANCE_RATIONALE: If meaning-primacy instruction disappeared overnight, teacher-preparation curricula, textbook markets, licensure exam content, and classroom assessment practices tied to running records and leveled texts would all require restructuring; a substantial cohort of already-credentialed teachers would need retraining, and the professional identity and scholarly output built around the framework would be significantly devalued.
% FOUNDING_PROBLEM: Mid-20th-century reading instruction was frequently rote, drill-based, and disconnected from meaning or motivation, producing technically decoding but disengaged readers; whole language arose to reconnect reading with authentic communication, comprehension, and a child's intrinsic interest in text, and to counter what its founders saw as a mechanistic, joyless view of literacy.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language faculty and curriculum publishers attest the meaning-and-motivation problem remains live and central to reading engagement. Independent cognitive science researchers (outside the beneficiary set), dyslexia advocacy organizations, and multiple national reading panels attest that the specific claim this reading rests on — that decoding emerges naturally from print exposure without explicit instruction — has been substantially falsified by phonological-processing research, and that continued adoption reflects institutional inertia in teacher-preparation programs rather than an unresolved pedagogical problem.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.58 (rising from 0.34 at the interval's start) because the harm to non-benefiting populations accumulated as cognitive science evidence on phonological processing hardened over the same decades that the pedagogy remained institutionally entrenched — a mismatch between accumulating disconfirming evidence and unchanging practice widens effective extraction. Suppression (0.52) reflects that alternative approaches (systematic phonics) faced real institutional resistance in schools of education and licensure requirements, though not always through direct coercion — more often through gatekeeping over what counts as credentialed, legitimate practice. Theater ratio (0.44) captures that a meaningful share of the framework's continued defense (invoking motivation and 'joy of reading' rhetoric) has shifted from substantive pedagogical argument toward defending institutional and professional legitimacy as contrary evidence mounted.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed literacy faculty and curriculum publishers sit near the full-beneficiary end: they set the terms of legitimacy and collect professional and commercial returns from its continuation. Fluent readers with home literacy support benefit incidentally — they would likely learn to read under nearly any method, so the constraint is close to costless for them and its apparent success is partly an artifact of their pre-existing advantage. Struggling decoders, dyslexic students, ELLs, and print-poor low-income students are structural targets: their reading acquisition outcomes depend specifically on the explicit instruction this method does not supply, and as powerless, trapped-exit agents in a system they cannot select out of, their effective extraction sits near the full-target end regardless of the modest raw base extractiveness score.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rote, meaning-disconnected drill instruction — was a genuine pedagogical problem in its era. Whether that problem remains live is contested: cognitive science research and structured-literacy advocates argue the founding problem has been substantially superseded by evidence that a synthesis (explicit decoding plus rich meaning engagement) resolves both concerns simultaneously, while whole-language faculty maintain the founding concern about joyless, disconnected literacy instruction remains active. Classifying this as tangled_rope rather than snare or mountain avoids two errors: treating it as pure extraction (it did solve a real problem for many learners and remains genuinely coordinating for children who arrive with strong pre-literacy resources) and treating it as natural, uncontested pedagogical consensus (which the declared beneficiaries and researcher exclusion demonstrate it is not).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_acquisition_analogy_validity,
    'Is the core theoretical premise — that decoding emerges naturally from print exposure the way oral language emerges from spoken exposure — empirically sound, or has it been substantially falsified by phonological-processing research?',
    'Systematic review of longitudinal reading-outcome studies comparing meaning-primacy versus explicit-phonics instruction across populations with varying phonological processing profiles, weighted toward studies with random assignment or strong quasi-experimental design.',
    'If substantially falsified, the coordination function claimed by this reading (a legitimate theory of literacy acquisition) is undermined, and the constraint''s persistence is better explained by institutional inertia and professional identity lock-in than genuine pedagogical merit — strengthening the case for reclassification toward snare for the affected subpopulations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_acquisition_analogy_validity, empirical, 'Whether the reading''s foundational cognitive-acquisition premise holds up against phonological-processing evidence.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given four declared readings of the reading_acquisition_legitimacy kernel (whole_language, phonics_decoding, balanced_literacy, structured_literacy_remediation), is the whole_language reading best understood as a historically superseded position now sustained mainly by institutional inertia, or as a live, contestable pedagogical philosophy still defensible on its own terms (e.g., motivation and comprehension-engagement outcomes not fully captured by decoding-accuracy metrics)?',
    'Track adoption trends in teacher-preparation programs and state literacy legislation over time; if adoption is monotonically declining in favor of structured-literacy and balanced approaches, that trend itself is evidence for the superseded-position framing.',
    'Determines whether the constraint''s classification should trend toward piton (a persisting institutional structure whose original coordination function has substantially atrophied) or remain tangled_rope (still actively serving some real population well).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is a live pedagogical position or an inertial holdover documented for genealogical completeness.').

omega_variable(
    professional_identity_lock_mechanism,
    'Is the resistance among credentialed literacy faculty and trained teachers to structured-literacy evidence best explained by genuine unresolved scientific disagreement, or by professional identity fusion (careers, publications, and institutional accreditation built on the framework making its abandonment personally and institutionally costly independent of the evidence)?',
    'Compare rates of practice change among literacy faculty with tenure/publication investment in the framework versus early-career or non-specialist educators exposed to the same evidence.',
    'If identity-lock dominates, the effective suppression experienced by struggling readers is higher than the raw suppression score suggests, since the mechanism sustaining the constraint is professional self-preservation rather than contested pedagogical merit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_identity_lock_mechanism, empirical, 'Whether faculty resistance to disconfirming evidence reflects genuine scientific dispute or institutional identity lock-in.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0, 0.18).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 8, 0.24).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 16, 0.31).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 24, 0.37).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 32, 0.41).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(read_be_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(read_be_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(read_su_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(read_su_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language concept 'legitimate reading instruction' per the ε-invariance principle. Each reading of the reading_acquisition_legitimacy kernel — whole_language_meaning_primacy (this story), phonics_decoding_primacy, balanced_literacy_integration, and structured_literacy_remediation — is authored as a separate constraint with its own ε, beneficiary/victim structure, and classification, because measuring 'legitimacy' through each reading's own lens yields substantially different extraction profiles. This story's ε (0.58) reflects meaning-primacy's substantial mismatch with phonological-processing needs for a large minority of learners; phonics_decoding_primacy would be authored with a lower ε reflecting closer alignment with converging cognitive-science evidence, though its own story may surface different beneficiary structures (e.g., commercial phonics-program publishers) worth documenting independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
