% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Systematic Phonics Instruction as Foundational Reading Mechanism
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the phonics reading of the contested
 *   reading-acquisition kernel: reading acquisition requires explicit,
 *   systematic instruction in grapheme-phoneme correspondence as a
 *   foundational skill, distinct from and not reducible to exposure-based or
 *   hybrid accounts. As state legislatures increasingly mandate 'science of
 *   reading' aligned curricula, this reading is enforced through
 *   curriculum-approval lists, teacher licensure requirements, and district
 *   compliance audits — moving from a research finding to an actively
 *   administered instructional mandate. The extraction is low but real and
 *   rising: it falls mainly on incumbent whole-language-trained teachers and
 *   leveled-reader publishers whose professional and commercial position
 *   depended on a different pedagogical account being dominant. Sibling
 *   readings (whole_language_reading, balanced_literacy_reading) are separate
 *   constraints with their own ε and stakeholder structures — this story does
 *   not average over them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.28).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.42).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Systematic Phonics Instruction as Foundational Reading Mechanism").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '4de25f8b-816c-4880-9b5d-e778e0e51876').
narrative_ontology:cs_kernel_codification('4de25f8b-816c-4880-9b5d-e778e0e51876', distributed).
narrative_ontology:cs_authority_grounding('4de25f8b-816c-4880-9b5d-e778e0e51876', expertise).
narrative_ontology:cs_interpretation_layer_present('4de25f8b-816c-4880-9b5d-e778e0e51876').
narrative_ontology:cs_reading_relation('4de25f8b-816c-4880-9b5d-e778e0e51876', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('4de25f8b-816c-4880-9b5d-e778e0e51876', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('4de25f8b-816c-4880-9b5d-e778e0e51876', foundational, decoding_requires_explicit_instruction_not_implicit_exposure).
narrative_ontology:cs_axiom_status(decoding_requires_explicit_instruction_not_implicit_exposure, holdable).
narrative_ontology:cs_axiom_grounding('4de25f8b-816c-4880-9b5d-e778e0e51876', decoding_requires_explicit_instruction_not_implicit_exposure, empirically_contingent).
narrative_ontology:cs_axiom('4de25f8b-816c-4880-9b5d-e778e0e51876', secondary, systematic_sequence_must_precede_authentic_text_immersion).
narrative_ontology:cs_axiom_status(systematic_sequence_must_precede_authentic_text_immersion, holdable).
narrative_ontology:cs_axiom_grounding('4de25f8b-816c-4880-9b5d-e778e0e51876', systematic_sequence_must_precede_authentic_text_immersion, instrumental).
narrative_ontology:cs_reference_frame('4de25f8b-816c-4880-9b5d-e778e0e51876', cognitive_science_decoding_evidence_base).
narrative_ontology:cs_drift_state('4de25f8b-816c-4880-9b5d-e778e0e51876', post_science_of_reading_legislative_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4de25f8b-816c-4880-9b5d-e778e0e51876', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, novice_teachers_using_scripted_curricula).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_whole_language).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, curriculum_publishers_of_leveled_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, schools_with_low_instructional_flexibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, novice_teachers_using_scripted_curricula).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children who lack strong phonological awareness and cannot reliably infer decoding patterns from context or exposure alone. Systematic grapheme-phoneme instruction gives them an explicit, teachable path to decoding that implicit exposure methods do not reliably provide. They have no say in which pedagogy their school adopts and bear the long-term cost of being taught the wrong way if the reading fails to be adopted.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Students whose neurological profile makes implicit pattern extraction from text especially unreliable. The research base most strongly and specifically supports explicit, systematic, cumulative phonics instruction for this group; absence of it produces measurable and often irreversible early reading gaps.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, national).

% Early-career teachers benefit from a scope-and-sequence that specifies what to teach and when, reducing the expertise burden of diagnosing individual decoding gaps. They also pay a cost in reduced professional discretion and in the up-front labor of learning an unfamiliar systematic method if they were trained differently.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, novice_teachers_using_scripted_curricula, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, novice_teachers_using_scripted_curricula, payer).

% Veteran teachers whose professional identity, credentialing, and years of classroom practice were built around meaning-centered, exposure-based methods. Mandated systematic phonics instruction requires retraining, discards accumulated professional judgment about individualized reading instruction, and in some jurisdictions is enforced through curriculum audits that treat their prior practice as noncompliant.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_whole_language, payer,
    moderate, biographical, constrained, regional).

% Publishers whose catalogs and business models are built on predictable, leveled, high-interest texts intended for exposure-based practice rather than decodable, phonetically controlled sequences. Mandated phonics adoption shifts procurement dollars toward decodable-text publishers; they can retool their catalogs, but at real transitional cost and lost market share.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, curriculum_publishers_of_leveled_readers, payer,
    organized, biographical, mobile, national).

% Underfunded districts that must absorb the high upfront cost of retraining staff, replacing materials, and restructuring literacy blocks around a systematic scope-and-sequence, often on compressed state-mandated timelines and without the budget flexibility that wealthier districts have.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, schools_with_low_instructional_flexibility, payer,
    moderate, biographical, constrained, regional).

% Cognitive scientists and literacy researchers who study decoding acquisition through controlled studies, meta-analyses (e.g. National Reading Panel-style syntheses), and longitudinal outcome tracking. They generate the evidence base cited to justify the mandate and are largely external to the classroom-level costs of implementing it.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, reading_science_researchers, observer,
    institutional, generational, analytical, global).

% State legislatures and departments of education that mandate 'science of reading' aligned instruction, certify curricula, and audit district compliance. They set the enforcement mechanism (curriculum approval lists, teacher licensure requirements) and bear little direct classroom-level cost themselves.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, state_education_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, empirically-grounded, cumulative sequence for teaching the mapping between written graphemes and spoken phonemes, so that decoding — the mechanical bottleneck to reading — is taught explicitly rather than left to be inferred, which reliably fails for a substantial minority of learners including most students with dyslexia.
% TRANSFER_FUNCTION: Moves instructional authority and curriculum-adoption resources away from whole-language-trained teachers and leveled-reader publishers toward decodable-text publishers, phonics-curriculum vendors, and teachers credentialed in systematic instruction; moves early-literacy outcomes disproportionately toward struggling readers and students with dyslexia who benefit most from explicit sequencing.
% ABSENT_VOICES: Whole-language-trained veteran teachers who believe their professional judgment about individualized, meaning-centered instruction is being overridden by policy mandate rather than persuaded by evidence; they are present in professional development sessions but largely absent from the state-level curriculum-approval and legislative processes that set the mandate.
% DISAPPEARANCE_RATIONALE: If the mandate for systematic phonics instruction were removed overnight, districts would revert to a mix of balanced literacy and whole-language approaches within one to two curriculum cycles, decodable-text publishers would lose their newly captured market share, and the measurable decline in early-grade phonics-specific skill outcomes documented pre-mandate would likely reassert itself for the subset of students who do not acquire decoding through implicit exposure.
% FOUNDING_PROBLEM: A persistent and measurable gap existed between how children were being taught to read (cueing/exposure-based methods emphasizing meaning-guessing from context and pictures) and what decades of cognitive science research showed about how the brain actually acquires decoding — producing a subset of otherwise capable children, disproportionately including those with dyslexia, who never became fluent decoders under implicit methods.
% FOUNDING_PROBLEM_CORROBORATION: Independent cognitive science researchers (outside the phonics curriculum and testing industry) and longitudinal outcome studies (e.g., NAEP reading score analyses, dyslexia intervention research) corroborate that the decoding gap was real and that systematic phonics instruction closes it for most affected students. Whole-language and balanced-literacy practitioners, largely from outside the phonics-curriculum-vendor beneficiary set, contest whether the problem is now adequately resolved for the majority of typically-developing readers or whether the mandate has overcorrected past the point the original evidence supports.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.28) because the coordination function is substantial and well-evidenced for the beneficiary population (struggling readers, students with dyslexia), and the costs borne by incumbent teachers and publishers are transitional retraining/retooling costs rather than ongoing rent extraction. Suppression is authored higher (0.42, rising to 0.42 by interval end) because the mandate is increasingly enforced through curriculum audits and licensure requirements that treat prior whole-language practice as noncompliant, narrowing teacher discretion regardless of individual classroom context. Theater ratio is low (0.15) — the instructional mechanism is functionally active, not primarily performative, though rising slightly as compliance paperwork accumulates around implementation.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers and students with dyslexia are structural beneficiaries with the least power and least exit — they cannot choose their school's curriculum and depend entirely on it being correct, so they sit near the full-beneficiary end when the reading is adopted well and are harmed most when it is adopted poorly or not at all. Teachers trained in whole language and leveled-reader publishers are structural payers: their professional capital and business models are devalued by the mandate, and their exit options are constrained (retraining, retooling) rather than blocked outright — this keeps them out of full-target territory but clearly on the paying side. State education agencies are agenda-setters with arbitrage-grade exit: they can shift mandates without bearing the classroom-level implementation costs themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a real, measurable decoding gap under exposure-based methods — has strong corroboration from outside the phonics-curriculum-vendor beneficiary class, which argues against treating this as a captured mandate riding on an exhausted justification. However, the founding_problem_status is authored as contested rather than clearly live: the original evidence base most strongly supports explicit instruction for students who struggle with implicit decoding acquisition, not necessarily for the full population of typically-developing readers for whom the marginal benefit of a fully scripted systematic sequence over lighter-touch approaches is less settled. This is exactly the divergence the framework is built to surface — the reading is claimed as rope (coordination benefiting a target population) while carrying rising suppression as it generalizes from a well-evidenced subpopulation claim to a universal classroom mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalization_beyond_evidenced_population,
    'Does the strong evidence base for systematic phonics instruction with struggling readers and students with dyslexia justify mandating it as the universal foundational method for all readers, including those who might acquire decoding adequately through lighter-touch or hybrid approaches?',
    'Meta-analyses that stratify outcomes by baseline phonological awareness and decoding risk, comparing systematic-phonics-for-all mandates against risk-differentiated instructional models (e.g., tiered/RTI approaches) on both average outcomes and equity of outcomes across the full ability distribution.',
    'If the marginal benefit for already-strong readers is negligible or even mildly negative for engagement/motivation, then the extractiveness borne by teachers and publishers is paying for coordination benefit concentrated in a subpopulation while the mandate''s suppression (narrowed discretion) is scoped to the entire population — widening the gap between the reading''s justification and its enforcement scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generalization_beyond_evidenced_population, empirical, 'Whether universal mandate scope is justified by evidence specific to at-risk decoding populations.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the phonics_reading diverge from balanced_literacy_reading — is it a genuine claim that explicit instruction must be foundational and sequenced BEFORE authentic text exposure, or merely a disagreement about proportion and timing within an instructional approach both readings could accept?',
    'Compare the two readings'' scope-and-sequence specifications directly: does phonics_reading require decodable-only texts during the foundational phase (excluding leveled/authentic texts until phonics is established), while balanced_literacy_reading permits concurrent authentic-text exposure from the start? That specific sequencing claim is the structural disagreement location.',
    'If the disagreement is genuinely about sequencing and exclusivity during the foundational phase, phonics_reading and balanced_literacy_reading are best modeled as coexisting policy positions rather than foreclosing one another — supporting the coexists_with relation rather than forecloses in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural axis of disagreement between phonics and balanced literacy readings.').

omega_variable(
    suppression_mechanism_teacher_discretion,
    'Is the suppression experienced by whole-language-trained teachers primarily structural (curriculum audits, licensure requirements, mandated materials) or partly internalized (professional identity built around a pedagogical philosophy now treated as discredited)?',
    'Survey and interview data tracking teachers post-retraining: if reported discomfort/resistance persists after successful adoption of new methods and after audits are lifted, that indicates an internalized identity component beyond the structural compliance mechanism.',
    'If substantially internalized, the effective suppression on this stakeholder group is understated by the structural measure alone — professional identity dynamics would persist even if enforcement were relaxed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_teacher_discretion, empirical, 'Structural versus internalized suppression mechanism for incumbent teachers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_mechanism__phonics_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__phonics_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_mechanism__phonics_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__phonics_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__phonics_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(read_be_t4, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 4, 0.21).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(read_be_t12, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(read_su_t4, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(read_su_t12, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__phonics_reading, 0.03).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'how reading acquisition works' (the reading_acquisition_mechanism kernel) into structurally distinct pedagogical claims, each with its own ε, beneficiary/victim structure, and classification: phonics_reading (this story — explicit systematic instruction as necessary foundation), whole_language_reading (implicit acquisition through authentic text exposure), and balanced_literacy_reading (integrated explicit-plus-authentic practice). The three are linked bidirectionally via affects_constraints because policy adoption of one directly displaces resources, curriculum, and teacher training investment from the others — they compete for the same instructional-time and procurement budget within a school system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
