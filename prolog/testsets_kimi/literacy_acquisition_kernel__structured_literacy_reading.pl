% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__structured_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Instruction Mandate (Orton-Gillingham)
 *   domain: educational/psychological/pedagogical
 *
 * SUMMARY:
 *   Structured literacy, rooted in the Orton-Gillingham tradition, posits
 *   that reading acquisition requires explicit, systematic, cumulative
 *   instruction across five components (phonological awareness, phonics,
 *   fluency, vocabulary, comprehension). Originally developed for students
 *   with dyslexia, its advocates argue for universal application. The
 *   constraint manifests as state curriculum mandates, teacher certification
 *   requirements, and approved instructional material lists. General
 *   education teachers bear the training burden, while students with dyslexia
 *   benefit from targeted intervention. The specialized certification
 *   industry occupies a beneficiary position alongside students, creating a
 *   tangled structure where genuine coordination for disabled readers is
 *   coupled with asymmetric extraction from the teaching workforce.
 *
 * KEY AGENTS:
 *   - students_with_dyslexia: Primary beneficiary (powerless/constrained) â receive systematic intervention
 *   - general_education_teachers: Primary target/payer (moderate/constrained) â bear certification costs and pedagogical compliance burden
 *   - state_education_agencies: Agenda setter (institutional/mobile) â mandate standards and licensure conditions
 *   - specialized_literacy_trainers: Secondary beneficiary (organized/mobile) â collect revenue from mandated training
 *   - whole_language_advocates: Excluded alternative (organized/constrained) â pedagogical framework suppressed by mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Instruction Mandate (Orton-Gillingham)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational/psychological/pedagogical").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, 'd5707cfa-4e6c-4b3e-9de5-93c404260a09').
narrative_ontology:cs_kernel_codification('d5707cfa-4e6c-4b3e-9de5-93c404260a09', fixed_text).
narrative_ontology:cs_authority_grounding('d5707cfa-4e6c-4b3e-9de5-93c404260a09', lineage).
narrative_ontology:cs_interpretation_layer_present('d5707cfa-4e6c-4b3e-9de5-93c404260a09').
narrative_ontology:cs_reading_relation('d5707cfa-4e6c-4b3e-9de5-93c404260a09', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('d5707cfa-4e6c-4b3e-9de5-93c404260a09', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('d5707cfa-4e6c-4b3e-9de5-93c404260a09', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('d5707cfa-4e6c-4b3e-9de5-93c404260a09', foundational, explicit_cumulative_five_component_instruction_universal).
narrative_ontology:cs_axiom_status(explicit_cumulative_five_component_instruction_universal, holdable).
narrative_ontology:cs_axiom_grounding('d5707cfa-4e6c-4b3e-9de5-93c404260a09', explicit_cumulative_five_component_instruction_universal, empirically_contingent).
narrative_ontology:cs_axiom('d5707cfa-4e6c-4b3e-9de5-93c404260a09', foundational, specialized_og_certification_required_for_fidelity).
narrative_ontology:cs_axiom_status(specialized_og_certification_required_for_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('d5707cfa-4e6c-4b3e-9de5-93c404260a09', specialized_og_certification_required_for_fidelity, instrumental).
narrative_ontology:cs_reference_frame('d5707cfa-4e6c-4b3e-9de5-93c404260a09', explicit_systematic_cumulative_instruction).
narrative_ontology:cs_drift_state('d5707cfa-4e6c-4b3e-9de5-93c404260a09', science_of_reading_movement_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d5707cfa-4e6c-4b3e-9de5-93c404260a09', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, specialized_literacy_trainers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, phonological_deficit_model).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, explicit_instruction_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive intensive, systematic, multisensory reading instruction matched to phonological processing deficits; reading outcomes depend on district adoption and teacher fidelity to the scope and sequence. Cannot opt out of the public education framework that assigns them to specific programs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, constrained, national).

% Required to complete specialized certification and ongoing professional development in structured literacy methods, often at personal expense or during unpaid time; must adopt prescribed curricula and pacing guides even where they conflict with existing classroom practice or prior training.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, national).

% Mandate evidence-based reading curricula, approve instructional materials aligned with structured literacy principles, and condition teacher licensure on completion of approved training programs. Can alter standards but face political pressure from parent advocacy groups.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, state_education_agencies, agenda_setter,
    institutional, generational, mobile, national).

% Provide accredited Orton-Gillingham and structured literacy certification courses to teachers and districts; revenue scales directly with mandate breadth and renewal requirements; act as gatekeepers for fidelity credentials and approved provider lists.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, specialized_literacy_trainers, beneficiary,
    organized, biographical, mobile, national).

% Their preferred instructional framework is systematically excluded from state-approved curriculum lists and teacher preparation programs; characterized in policy discourse as ideologically motivated or anti-science despite decades of classroom presence.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, whole_language_advocates, excluded,
    organized, biographical, constrained, national).

% Conduct meta-analyses and RCTs on reading instruction; supply empirical warrants that state agencies cite. Occupy a seat outside the training market and classroom implementation chain, though funding sources may align with contested traditions.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, independent_reading_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, specialized_literacy_trainers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates consistent, evidence-based reading instruction across classrooms so that students with phonological processing deficits receive systematic, cumulative exposure to phoneme-grapheme correspondence rather than inconsistent or implicit approaches.
% TRANSFER_FUNCTION: Transfers credentialing fees and training time from general education teachers to specialized literacy trainers; transfers systematic decoding instruction and its associated outcomes to students with dyslexia and learning disabilities.
% ABSENT_VOICES: Whole language advocates and experienced general education teachers who view the certification burden as excessive or the scope-and-sequence approach as overly rigid are structurally excluded from curriculum adoption panels and licensure boards.
% DISAPPEARANCE_RATIONALE: Districts would revert to heterogeneous instructional materials and local pedagogical preference; the specialized training market would contract sharply; students with dyslexia would lose guaranteed access to systematic intervention; teacher professional development budgets would be reallocated.
% FOUNDING_PROBLEM: A substantial subset of students, particularly those with dyslexia, fail to acquire reading through implicit or incidental instruction due to phonological processing deficits; without explicit, systematic, cumulative decoding instruction, these students experience disproportionate academic failure and collateral psychosocial harm.
% FOUNDING_PROBLEM_CORROBORATION: Independent cognitive neuroscience research on dyslexia and large-scale RCTs on reading intervention corroborate the phonological deficit model and the efficacy of explicit phonics. Special education advocacy organizations (e.g., Decoding Dyslexia) attest to the live status from a beneficiary-adjacent but non-commercial seat. The specialized literacy training industry also attests to it, but their commercial interest is noted.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is driven by the teacher certification layer: the coordination gain for dyslexic readers is real but decoupled from the training burden imposed on all teachers. Suppression (0.75) reflects the active exclusion of whole language and balanced literacy from state-approved curricula and teacher prep programs. Theater ratio (0.45) captures districts purchasing OG-branded materials and certifying teachers without achieving fidelity of implementation. Accessibility collapse (0.70) is high because once the science-of-reading frame dominates policy, alternatives lose legitimacy rapidly. Resistance (0.50) reflects ongoing pushback from whole language holdouts, teacher unions, and balanced literacy researchers.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state agencies) experiences the constraint as solving a documented educational failure; the payer seat (general education teachers) experiences it as an unfunded credentialing mandate; the beneficiary seat (students with dyslexia) experiences it as necessary support. The engine should compute these seats differently: low directionality for students and trainers, high directionality for teachers.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with dyslexia are structural beneficiaries (constraint subsidizes their access to intervention; d near beneficiary end). Specialized literacy trainers are also beneficiaries, collecting rents from the certification layer (d near beneficiary end). General education teachers are structural targets: they pay in time and money for training whose marginal benefit to their own students is contested (d near target end). State education agencies sit near symmetric: they enforce but do not personally collect; they gain political cover from the science-of-reading movement.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (a snare on teachers) by preserving the genuine coordination function for dyslexic readers, while also preventing mislabeling it as pure coordination (a rope) by acknowledging the asymmetric certification burden. If the dyslexia-specific intervention were cleanly separable from the universal mandate, the universal layer would likely compute as a snare; because the same structure delivers both, the type is tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structured_phonics_distinctness,
    'Is structured literacy a distinct reading of the literacy acquisition kernel, or merely a variant of phonics_reading with an appended certification-extraction apparatus?',
    'Comparative structural analysis: if the OG certification and five-component scope add independent extractiveness not present in the phonics_reading constraint, the two are distinct constraints; if removing the certification layer collapses the structure into phonics_reading, they are the same kernel reading.',
    'If distinct, the kernel carries four live readings; if variant, the training burden is an extrinsic policy layer and the kernel carries three readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_phonics_distinctness, conceptual, 'Whether structured literacy is an independent reading or a phonics variant').

omega_variable(
    universality_extraction_boundary,
    'Does universal mandatory application of structured literacy produce coordination gains for general education students beyond what targeted intervention produces, or does universalization extract teacher capacity without marginal benefit?',
    'RCTs and quasi-experimental studies comparing universal structured literacy implementation against targeted intervention for at-risk readers within general education populations.',
    'If universal application shows no marginal gain over targeted intervention, the general-education mandate functions as extraction from teachers; if broad gains accrue, the coordination function justifies universality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_extraction_boundary, empirical, 'Whether universal mandate is coordination or extraction').

omega_variable(
    alternative_pedagogy_suppression,
    'Is the suppression of whole language and balanced literacy alternatives empirically warranted by outcome data, or structurally enforced to protect the legitimacy and market position of structured literacy certification?',
    'Examine whether suppression intensity tracks reading outcome differentials or tracks certification market expansion; compare jurisdictions with and without mandated structured literacy exclusivity.',
    'If suppression tracks market expansion more closely than outcome differentials, the constraint operates partly as a snare on pedagogical pluralism; if suppression tracks genuine efficacy gaps, it is warranted coordination enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pedagogy_suppression, empirical, 'Whether suppression of alternatives is warranted or market-protective').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel decomposes into four structurally distinct readings: whole_language_reading (implicit emergence), phonics_reading (decoding primacy), balanced_literacy_reading (complementary balance), and structured_literacy_reading (explicit systematic cumulative instruction with specialized certification). Each reading has a distinct epsilon, beneficiary/victim structure, and institutional footprint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
