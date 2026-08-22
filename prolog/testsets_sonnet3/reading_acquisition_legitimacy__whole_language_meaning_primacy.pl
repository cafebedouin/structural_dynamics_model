% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: Whole Language / Meaning-Primacy Reading Instruction Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint captures the meaning-primacy (whole language) reading
 *   instruction paradigm: the doctrine that reading is fundamentally
 *   meaning-making, analogous to oral language acquisition, and that
 *   legitimate instruction immerses children in authentic literature while
 *   decoding skills emerge naturally through exposure rather than explicit
 *   teaching. This is one reading of a contested kernel about what counts as
 *   legitimate reading-acquisition instruction; sibling readings
 *   (phonics_decoding_primacy, balanced_literacy_integration,
 *   structured_literacy_remediation) are separate constraint stories, not
 *   alternate framings of this one. The ε authored here is for the
 *   meaning-primacy arrangement as this reading's own advocates and critics
 *   would assess its actual operation — not for what phonics or
 *   balanced-literacy advocates would say about it, and not for the
 *   rights-respecting alternative this reading's critics would install.
 *
 * KEY AGENTS:
 *   - whole_language_curriculum_publishers: beneficiary (organized/arbitrage) — sell materials and PD tied to the framework
 *   - credentialed_whole_language_teacher_educators: agenda_setter/beneficiary (institutional/identity_locked) — set certification standards, professionally and identity-invested
 *   - children_with_dyslexia_or_phonological_deficits: primary target (powerless/trapped) — bear the extraction of masked, undiagnosed decoding failure
 *   - children_from_low_print_exposure_households: target (powerless/trapped) — lack the home-literacy scaffolding the method assumes
 *   - english_language_learners: target (powerless/trapped) — contextual-guessing assumptions fail when oral vocabulary is still forming
 *   - classroom_teachers_trained_only_in_meaning_primacy_methods: payer/agenda_setter (moderate/constrained) — implement the mandate, blamed for its outcomes, undertrained to remediate
 *   - reading_science_researchers: excluded (institutional/analytical) — converging cognitive-science evidence long absent from teacher preparation
 *   - school_district_administrators: observer (institutional/constrained) — weigh switching costs against mounting legislative and evidentiary pressure
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
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language / Meaning-Primacy Reading Instruction Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '230b6102-d038-44dc-aca0-9baaef08a838').
narrative_ontology:cs_kernel_codification('230b6102-d038-44dc-aca0-9baaef08a838', distributed).
narrative_ontology:cs_authority_grounding('230b6102-d038-44dc-aca0-9baaef08a838', practice).
narrative_ontology:cs_interpretation_layer_present('230b6102-d038-44dc-aca0-9baaef08a838').
narrative_ontology:cs_reading_relation('230b6102-d038-44dc-aca0-9baaef08a838', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('230b6102-d038-44dc-aca0-9baaef08a838', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('230b6102-d038-44dc-aca0-9baaef08a838', foundational, decoding_emerges_from_authentic_text_immersion).
narrative_ontology:cs_axiom_status(decoding_emerges_from_authentic_text_immersion, holdable).
narrative_ontology:cs_axiom_grounding('230b6102-d038-44dc-aca0-9baaef08a838', decoding_emerges_from_authentic_text_immersion, empirically_contingent).
narrative_ontology:cs_axiom('230b6102-d038-44dc-aca0-9baaef08a838', secondary, explicit_phonics_instruction_undermines_meaning_engagement).
narrative_ontology:cs_axiom_status(explicit_phonics_instruction_undermines_meaning_engagement, overridden).
narrative_ontology:cs_axiom_grounding('230b6102-d038-44dc-aca0-9baaef08a838', explicit_phonics_instruction_undermines_meaning_engagement, empirically_contingent).
narrative_ontology:cs_reference_frame('230b6102-d038-44dc-aca0-9baaef08a838', emergent_literacy_developmental_model).
narrative_ontology:cs_drift_state('230b6102-d038-44dc-aca0-9baaef08a838', post_science_of_reading_legislative_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('230b6102-d038-44dc-aca0-9baaef08a838', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, credentialed_whole_language_teacher_educators).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, fluent_readers_with_strong_home_literacy).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_with_dyslexia_or_phonological_deficits).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_from_low_print_exposure_households).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, english_language_learners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers_trained_only_in_meaning_primacy_methods).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_as_natural_language_acquisition_analog).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, meaning_construction_as_primary_reading_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce leveled readers, guided-reading kits, and running-records assessment systems sold to districts under the meaning-primacy framework. Revenue depends on the framework's continued adoption in teacher preparation and district curriculum mandates; they fund professional development that reinforces the paradigm.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Staff schools of education, write certification standards, and train new teachers in three-cueing and meaning-first strategies. Their professional identity, publication record, and institutional standing are built on this framework; abandoning it would mean repudiating decades of their own scholarship and teaching practice.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, credentialed_whole_language_teacher_educators, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, credentialed_whole_language_teacher_educators, beneficiary).

% Enter school with substantial oral vocabulary, print exposure, and often incidental phonemic awareness from home reading. They can infer words from context and pictures well enough that meaning-primacy instruction does not expose their decoding gaps; they read successfully under this method largely because they would read successfully under most methods.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, fluent_readers_with_strong_home_literacy, beneficiary,
    moderate, biographical, mobile, local).

% Cannot reliably guess words from context or pictures because their core deficit is phonological processing, not meaning-making. Under meaning-primacy instruction they are taught to use cueing strategies that mask rather than remediate their decoding gap, often not identified as struggling until years of instruction have passed and the gap has widened. They have no say in curriculum and cannot exit the classroom.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_with_dyslexia_or_phonological_deficits, payer,
    powerless, biographical, trapped, local).

% Enter school with less incidental phonological and print exposure than peers from print-rich homes. Meaning-primacy instruction assumes children will absorb the alphabetic code through exposure to authentic texts, which advantages children who already have that exposure at home and leaves these children without explicit compensatory instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_from_low_print_exposure_households, payer,
    powerless, biographical, trapped, local).

% Are learning English vocabulary and syntax simultaneously with reading; the meaning-primacy assumption that oral language competence supports contextual guessing does not hold when oral vocabulary itself is still developing. They are disproportionately identified as struggling readers under this model.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, english_language_learners, payer,
    powerless, biographical, trapped, local).

% Deliver the curriculum daily and are held accountable for reading outcomes, but were certified under programs that did not teach systematic phonics content knowledge. When students struggle, they lack the diagnostic and remediation tools structured-literacy training would provide, and often bear public and administrative blame for outcomes traceable to their own preparation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers_trained_only_in_meaning_primacy_methods, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers_trained_only_in_meaning_primacy_methods, agenda_setter).

% Cognitive scientists and reading researchers whose converging evidence on phonological processing and the simple view of reading has been part of the literature for decades but was for a long period structurally excluded from teacher-preparation curricula and district policy conversations dominated by literacy-education faculty.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_science_researchers, excluded,
    institutional, generational, analytical, national).

% Select curricula, respond to state literacy mandates and parent advocacy, and increasingly face legislative pressure (the 'science of reading' movement) to replace meaning-primacy programs. They weigh switching costs, teacher retraining, and political pressure from multiple constituencies.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, school_district_administrators, observer,
    institutional, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom practice, curriculum publishing, teacher certification standards, and assessment (running records, leveled texts) around a shared theory that reading is fundamentally an act of meaning construction analogous to oral language acquisition, allowing large systems of teacher preparation and materials production to align on a single instructional philosophy.
% TRANSFER_FUNCTION: Moves instructional time, remediation resources, and professional legitimacy toward meaning-based strategies (three-cueing, predictable texts, contextual guessing) and away from systematic, explicit phonics instruction; the cost of that reallocation falls disproportionately on children whose reading acquisition depends on explicit decoding instruction, while curriculum publishers and teacher-education faculty retain revenue and institutional authority tied to the framework.
% ABSENT_VOICES: Cognitive and reading-science researchers documenting the phonological basis of decoding difficulty were for decades largely absent from schools-of-education curricula and district in-service training; parents of struggling readers lacking specialized knowledge of reading science were often told their children's difficulties were developmental or attentional rather than attributable to instructional method.
% DISAPPEARANCE_RATIONALE: If meaning-primacy instruction and its supporting teacher-certification, curriculum-publishing, and assessment infrastructure vanished, districts would need to retrain large numbers of teachers, replace curricular materials, and rebuild certification standards around explicit phonics — a substantial institutional and financial rearrangement, which is exactly the resistance the ongoing 'science of reading' legislative movement has encountered.
% FOUNDING_PROBLEM: Mid-20th-century phonics instruction was often rote, decontextualized, and disconnected from comprehension and love of reading; whole language arose to solve a real problem of joyless, meaning-starved decoding drills that produced technically capable but disengaged readers.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language teacher educators and curriculum publishers attest the framework remains necessary to preserve authentic, motivating literacy experiences. Cognitive scientists, reading researchers outside the literacy-education establishment, and increasingly state legislatures (citing NAEP reading scores and dyslexia advocacy groups) attest that the founding problem of joyless phonics drilling could be solved without abandoning systematic decoding instruction, and that the meaning-primacy solution created a new, more severe problem for the substantial minority of children who cannot infer the alphabetic code from exposure alone.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.58 (moderate-high, not extreme): the framework does deliver real coordination value for the substantial share of children who acquire decoding incidentally, but it imposes serious, measurable costs on children whose reading acquisition depends on explicit phonological instruction — costs that compound over years before remediation is offered, if it is offered at all. Suppression is authored at 0.52 (moderate): the mechanism is less coercive law than pervasive institutional lock-in across certification, publishing, and school culture, which is real but softer than a legal mandate. Theater ratio (0.44) reflects that meaning-primacy practices retain a defensible authentic-literature-exposure function while an increasing share of activity (running records, guided-reading rituals) has become proxy compliance theater as the evidentiary case against unassisted decoding-emergence has hardened. Accessibility collapse is moderate (0.40): explicit-phonics alternatives were never fully extinguished as a body of research, but were structurally marginalized from teacher preparation for decades. Resistance is high (0.68) and rising, driven by the science-of-reading legislative movement, dyslexia advocacy, and increasingly organized parent groups.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries — curriculum publishers and teacher-educator faculty — sit near the full-beneficiary end: they collect revenue, credential authority, and professional legitimacy from the framework's continued dominance, with mobile or arbitrage-level exit (they can pivot products or scholarship if pressure mounts, and often do so gradually). Fluent readers from print-rich homes are incidental beneficiaries — the method does not harm them because their decoding gap was never exposed, not because the method serves them specifically. Victims — children with phonological deficits, low print exposure, and ELL status — sit near the full-target end: trapped exit options (no meaningful choice of instructional method within their assigned classroom), powerless standing, and the extraction compounds over their whole early education before intervention, if any, arrives. Classroom teachers occupy an intermediate, asymmetric position: they are simultaneously agents of the mandate's daily enforcement and payers of its reputational and professional cost, since public accountability for reading outcomes attaches to them despite their training gap being institutionally imposed, not chosen.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is deliberately chosen over snare: the framework retains a genuine coordination function (aligning literature exposure, teacher preparation, and materials at scale, and correctly diagnosing the joylessness of rote 1950s-60s phonics drilling as a real problem) alongside asymmetric extraction from a specific, identifiable, powerless population. Classifying this as pure snare would erase the real coordination value it provides for the majority of children who decode incidentally; classifying it as pure rope would erase the compounding, well-documented harm to children with phonological processing differences. The active-enforcement gate is satisfied by certification requirements, curriculum adoption mandates, and professional gatekeeping that make deviation costly for individual teachers even when they suspect a student needs explicit phonics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the whole_language_meaning_primacy reading diverge from its siblings — is the disagreement about the END STATE of skilled reading (which all four readings likely agree involves both meaning and decoding) or about the SEQUENCE AND EXPLICITNESS of instruction needed to get there?',
    'Comparative analysis of the four kernel readings'' instructional sequencing claims against longitudinal reading-outcome data disaggregated by phonological-processing profile, controlling for home literacy environment.',
    'If the disagreement is purely about sequencing/explicitness (not end-state), this reading and balanced_literacy_integration may be closer than the ''meaning vs. decoding'' framing suggests, which would change how much of the measured extraction is attributable to this reading specifically versus a broader implementation-fidelity problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the four kernel readings disagree about reading''s nature or about instructional method for a shared end-state.').

omega_variable(
    differential_harm_causal_attribution,
    'Is the elevated identification of reading difficulty among ELL and low-print-exposure children under meaning-primacy instruction caused by the instructional method itself, or does it reflect pre-existing disparities that would produce elevated identification under any instructional method absent additional scaffolding?',
    'Randomized or quasi-experimental comparison of matched populations (by home literacy environment, language status) receiving meaning-primacy versus structured/explicit phonics instruction, tracking decoding and comprehension outcomes over multiple years.',
    'If the method is the causal driver, extraction from these victim groups is directly attributable to this constraint''s operation. If disparities are substantially pre-existing and method-invariant, the extraction attributable to this specific reading (versus to broader educational inequality) should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differential_harm_causal_attribution, empirical, 'Causal attribution of differential reading outcomes to instructional method versus pre-existing disparity.').

omega_variable(
    identity_lock_reversibility,
    'How reversible is the identity-lock among teacher-educator faculty who built careers on meaning-primacy scholarship — would exposure to converging cognitive-science evidence produce genuine paradigm revision, or does institutional and professional sunk cost make the lock effectively permanent regardless of evidence?',
    'Track institutional responses (curriculum revision, faculty retraining, publication shifts) at schools of education following state-level science-of-reading legislative mandates, as a natural experiment in identity-lock reversibility under external pressure.',
    'If reversible under sufficient external pressure, the current classification''s suppression and resistance metrics should be read as a transitional peak rather than a stable equilibrium. If effectively permanent, the tangled_rope classification understates the extractive persistence mechanism and a drift toward snare is more likely as evidence accumulates without practice change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether teacher-educator identity investment in the framework is reversible under evidentiary and legislative pressure.').


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
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 16, 0.33).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 24, 0.38).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 32, 0.42).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(read_be_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(read_be_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 24, 0.46).
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
% This story is one of four constraints decomposing the natural-language concept 'legitimate reading instruction' per the epsilon-invariance principle: whole_language_meaning_primacy (this file), phonics_decoding_primacy, balanced_literacy_integration, and structured_literacy_remediation. Each reading has its own beneficiary/victim structure and its own epsilon; they are linked because they compete for the same institutional resources (teacher certification standards, curriculum adoption budgets, instructional time) and because legislative and evidentiary pressure on one reading directly affects resource availability and legitimacy for the others — the 'science of reading' movement functions as a shared downstream/upstream pressure across all four.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
