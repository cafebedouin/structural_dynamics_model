% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Explicit Systematic Phonics-First Reading Instruction Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the phonics-first reading of the literacy
 *   acquisition kernel: reading acquisition requires explicit, systematic
 *   phoneme-grapheme correspondence instruction before extensive
 *   connected-text exposure, with decoding treated as the precondition for
 *   comprehension. As state 'science of reading' legislation has translated
 *   this claim into scripted-curriculum mandates enforced through fidelity
 *   audits, the constraint has accumulated a coordination function (closing
 *   the decoding gap for students with weak phonological awareness) alongside
 *   an extraction function (compressing teacher professional discretion and
 *   imposing uniform pacing on students who do not need it). The claimed type
 *   is tangled_rope: a genuine coordination problem exists (many children
 *   fail to decode without explicit instruction) and a genuine extraction
 *   exists (teacher judgment and non-target-student needs are the resource
 *   spent to guarantee delivery fidelity at scale).
 *
 * KEY AGENTS:
 *   - students_with_weak_phonological_awareness: Primary beneficiary (powerless/trapped) — gains decoding competence from systematic instruction
 *   - classroom_teachers_professional_judgment: Primary target of extraction (moderate/constrained) — professional discretion is suppressed by scripted-fidelity mandates
 *   - curriculum_publishers_of_scripted_phonics_programs: Secondary beneficiary (organized/arbitrage) — captures mandate-driven adoption revenue
 *   - state_literacy_policy_offices: Agenda-setter (institutional/analytical) — writes and enforces the mandate, cites evidence base
 *   - reading_researchers_simple_view_tradition: Analytical observer (analytical/analytical) — some entangled with consulting relationships to publishers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.52).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.58).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Explicit Systematic Phonics-First Reading Instruction Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '23a036fb-81fa-4296-ba97-75d4942c03b9').
narrative_ontology:cs_kernel_codification('23a036fb-81fa-4296-ba97-75d4942c03b9', distributed).
narrative_ontology:cs_authority_grounding('23a036fb-81fa-4296-ba97-75d4942c03b9', expertise).
narrative_ontology:cs_interpretation_layer_present('23a036fb-81fa-4296-ba97-75d4942c03b9').
narrative_ontology:cs_reading_relation('23a036fb-81fa-4296-ba97-75d4942c03b9', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('23a036fb-81fa-4296-ba97-75d4942c03b9', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('23a036fb-81fa-4296-ba97-75d4942c03b9', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('23a036fb-81fa-4296-ba97-75d4942c03b9', foundational, decoding_is_causally_prior_to_comprehension).
narrative_ontology:cs_axiom_status(decoding_is_causally_prior_to_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('23a036fb-81fa-4296-ba97-75d4942c03b9', decoding_is_causally_prior_to_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('23a036fb-81fa-4296-ba97-75d4942c03b9', foundational, explicit_sequencing_must_precede_connected_text_exposure).
narrative_ontology:cs_axiom_status(explicit_sequencing_must_precede_connected_text_exposure, holdable).
narrative_ontology:cs_axiom_grounding('23a036fb-81fa-4296-ba97-75d4942c03b9', explicit_sequencing_must_precede_connected_text_exposure, empirically_contingent).
narrative_ontology:cs_reference_frame('23a036fb-81fa-4296-ba97-75d4942c03b9', cognitive_science_decoding_precedence_model).
narrative_ontology:cs_drift_state('23a036fb-81fa-4296-ba97-75d4942c03b9', post_science_of_reading_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23a036fb-81fa-4296-ba97-75d4942c03b9', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_scripted_phonics_programs).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, state_literacy_policy_offices).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers_professional_judgment).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, advanced_readers_under_scripted_pacing).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, multilingual_learners_with_nonstandard_grapheme_mappings).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, decoding_precedes_comprehension).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, simple_view_of_reading).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children who struggle to segment and blend sounds benefit from explicit, cumulative phoneme-grapheme mapping instruction; without it, many would not crack the code at all. They have no say in curriculum choice and depend entirely on what their assigned classroom delivers.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, regional).

% Trained to read student cues and differentiate instruction, teachers are required under phonics mandates to follow scripted lesson sequences with fidelity, often audited by walkthrough checklists. Deviating from the sequence — even for a student who would benefit from a different approach — risks negative evaluation. Their professional discretion is the resource the mandate spends.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers_professional_judgment, payer,
    moderate, biographical, constrained, regional).

% Children who already infer phonics patterns from exposure sit through drills calibrated for struggling decoders. Time that could go to comprehension, vocabulary, or independent reading is spent on skills they have already mastered, with no room in the schedule to differentiate upward.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, advanced_readers_under_scripted_pacing, payer,
    powerless, immediate, trapped, local).

% Programs built around standard English grapheme-phoneme correspondences do not account for transfer effects from students' first-language orthographies. These students are drilled on a mapping system that partially mismatches their existing phonological categories, and the scripted program has no built-in accommodation.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, multilingual_learners_with_nonstandard_grapheme_mappings, payer,
    powerless, biographical, trapped, regional).

% Sell packaged scope-and-sequence phonics curricula, decodable text sets, and mandated assessment kits to districts required by state law to adopt 'evidence-based' reading instruction. Revenue scales directly with the breadth and rigidity of adoption mandates.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_scripted_phonics_programs, beneficiary,
    organized, generational, arbitrage, national).

% Write and enforce 'science of reading' legislation mandating phonics-first instruction, tying district funding and teacher certification requirements to compliance. They point to NAEP and dyslexia-prevalence data as justification and administer the compliance audits districts must pass.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, state_literacy_policy_offices, agenda_setter,
    institutional, generational, analytical, national).

% Publish and synthesize the decoding-and-comprehension evidence base underlying the mandate. Some serve as expert witnesses or consultants to policy offices and publishers, which complicates their claim to pure analytical distance from the constraint's beneficiary structure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_researchers_simple_view_tradition, observer,
    analytical, generational, analytical, global).

% Teachers and teacher-educators trained in meaning-first or balanced approaches have had their pedagogical tradition legislated out of permitted practice in many jurisdictions. They are not consulted in mandate design and their objections are treated as already-refuted rather than live input.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_and_balanced_literacy_practitioners, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, diffuse).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates instructional sequencing across an entire school system so that every teacher, regardless of individual training or belief, delivers a phoneme-grapheme correspondence curriculum before extensive connected-text exposure, closing the gap for students who would not otherwise decode reliably.
% TRANSFER_FUNCTION: Moves instructional time, teacher discretion, and curriculum-purchasing dollars away from locally-designed, differentiated literacy instruction and toward standardized scripted programs; moves decoding competence toward students with weak phonological awareness at the cost of pacing flexibility for others.
% ABSENT_VOICES: Whole-language and balanced-literacy practitioners, along with teachers who successfully differentiate instruction informally, are structurally excluded from the policy conversation once 'science of reading' legislation is enacted; their pedagogical judgment is treated as a solved question rather than live professional input.
% DISAPPEARANCE_RATIONALE: If the phonics-first mandate vanished overnight, districts would revert to locally determined literacy curricula within a budget cycle, curriculum publishers dependent on state-mandated adoption would lose a guaranteed revenue channel, and teachers would regain discretion to sequence phonics and meaning-based instruction according to individual classroom need — some students with weak phonological awareness would likely see instruction become less systematic again.
% FOUNDING_PROBLEM: Large cohorts of children, particularly those with dyslexia-spectrum profiles, were leaving whole-language and balanced-literacy classrooms unable to decode print reliably; national and state reading proficiency scores remained stubbornly low despite decades of meaning-first instructional dominance.
% FOUNDING_PROBLEM_CORROBORATION: Independent cognitive science researchers outside the curriculum-publishing industry (some testifying without financial ties to phonics programs) corroborate that decoding deficits were real and systematic instruction closes them for at-risk readers. However, independent literacy researchers and teacher-education faculty outside the policy-office and publisher ecosystem also attest that the mandate's rigid, one-size-fits-all delivery mechanism exceeds what the founding evidence actually supports, and that the scripted-fidelity enforcement apparatus serves publisher and compliance interests beyond the original decoding-deficit problem.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.30 to 0.52) as legislative mandates matured from voluntary adoption to fidelity-audited compliance regimes with certification consequences for non-compliant teachers. Theater ratio is moderate and rising (0.10 to 0.28) reflecting growing compliance-documentation overhead (walkthrough checklists, pacing-guide sign-offs) that increasingly measures adherence to the script rather than student decoding outcomes directly. Suppression rises correspondingly (0.35 to 0.58) as certification and evaluation consequences for deviation have hardened. Accessibility collapse is moderate (0.45): districts retain some latitude in program selection even under mandate, and some teachers retain informal differentiation practices despite audit pressure. Resistance is substantial (0.55): teacher-education faculty, some literacy researchers, and multilingual-education specialists actively contest the universalizing rigidity of the mandate even while accepting the underlying decoding-deficit evidence.
 *
 * PERSPECTIVAL GAP:
 *   From the state policy office's agenda-setting seat, the mandate looks like rope: a coordination solution to a well-documented, previously-unsolved decoding crisis. From the classroom teacher's payer seat, the same structure looks like a tangled rope shading toward snare: a genuine problem (some students need explicit instruction) has been used to justify near-total removal of professional discretion, audited and enforced regardless of whether a given student needs the scripted sequence. The engine should compute this divergence directly from the differing power/exit profiles of these two seats — the coordination story is not false for the beneficiary seat, but it is materially incomplete for the payer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with weak phonological awareness sit near the beneficiary end: the constraint subsidizes their decoding acquisition directly and they have no exit from whatever curriculum their school delivers, but the delivered content structurally serves their need. Classroom teachers sit near the target end: their professional judgment — previously a resource they deployed at discretion — is now the object the fidelity-audit apparatus extracts compliance from; their exit options are constrained by certification and employment stakes, not open. Advanced readers and multilingual learners are secondary targets: the uniform pacing extracts instructional time or mismatches their existing phonological categories without any coordination benefit accruing to them specifically. Curriculum publishers and policy offices are structural beneficiaries: the former captures adoption revenue, the latter captures political credit for measurable compliance, neither bearing the classroom-level cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — systematic decoding failure under meaning-first instruction — is only partially resolved as literacy outcomes improve unevenly across districts adopting the mandate; the founding_problem_status is authored as 'contested' rather than 'dead' or 'live' because researchers outside the publisher/policy ecosystem agree the original problem was real but dispute whether the current enforcement apparatus is still calibrated to it or has drifted into compliance theater serving publisher and political interests independent of decoding outcomes. This divergence between the founding justification and the current enforcement scope is exactly the tangled_rope signature the classification should surface, rather than collapsing to either pure rope (ignoring the extraction) or pure snare (ignoring the genuine coordination gain for the target beneficiary population).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonics_reading_kernel_reading_identity,
    'Is the phonics_reading claim (decoding precedes and enables comprehension, requiring explicit sequencing before connected-text exposure) a distinct empirical commitment from structured_literacy_reading''s claim (phonological awareness, phonics, fluency, vocabulary, and comprehension as co-equal explicit strands), or is phonics_reading simply a narrower/earlier version of the same underlying claim?',
    'Compare the two readings'' falsification conditions: phonics_reading is falsified by evidence that connected-text exposure alone (without prior explicit phoneme-grapheme sequencing) produces equivalent decoding outcomes; structured_literacy_reading is falsified by evidence that omitting any of its five co-equal strands does not degrade outcomes. If these falsification conditions differ, the readings are structurally distinct and each should retain independent classification.',
    'If phonics_reading collapses into structured_literacy_reading, this story''s beneficiary/victim structure and ε should be reconciled with the structured_literacy_reading story via network.affects_constraints rather than treated as fully independent; if they remain distinct, the current independent classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_reading_kernel_reading_identity, conceptual, 'Whether the phonics-precedence reading is a genuinely separate kernel reading or a subset of the structured literacy reading.').

omega_variable(
    teacher_autonomy_extraction_magnitude,
    'How much of the measured extraction from classroom teachers is attributable to the phonics-precedence sequencing claim itself, versus the fidelity-audit enforcement apparatus that could in principle exist independent of any specific pedagogical content claim?',
    'Compare teacher-autonomy outcomes in jurisdictions that mandate phonics-first content but do not enforce scripted fidelity audits against jurisdictions that enforce both content and delivery-method compliance.',
    'If most of the measured extraction traces to the audit-enforcement mechanism rather than the phonics-precedence claim itself, then the extraction is better modeled as a property of the enforcement instrument, not of this specific kernel reading, and ε should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_extraction_magnitude, empirical, 'Disentangling content-mandate extraction from delivery-fidelity-audit extraction.').

omega_variable(
    researcher_beneficiary_entanglement,
    'To what extent are the reading researchers whose evidence base underlies this mandate financially or professionally entangled with the curriculum publishers who benefit from mandate adoption, such that the ''analytical observer'' seat is partially captured?',
    'Disclosure audit of consulting relationships, royalty arrangements, and expert-witness fees between simple-view-of-reading researchers and phonics curriculum publishers named as beneficiaries in this story.',
    'High entanglement would suggest the observer seat''s corroboration of the founding problem is not fully independent of the beneficiary structure, weakening the founding_problem_corroboration claim and shifting the story''s classification pressure toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(researcher_beneficiary_entanglement, empirical, 'Whether the research evidence base is independent of or entangled with the curriculum-publishing beneficiary group.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__phonics_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__phonics_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__phonics_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__phonics_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lite_su_t4, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__phonics_reading, 0.1).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the literacy_acquisition_kernel, each authored as a separate ε-invariant constraint story per the ε-invariance principle. phonics_reading claims decoding precedence and explicit pre-text sequencing specifically; whole_language_reading denies explicit decoding instruction is necessary at all (a foreclosing relationship: the two core premises cannot coexist within one instructional framework); balanced_literacy_reading treats phonics and meaning-engagement as complementary (an influenced sibling: phonics_reading's evidentiary success pressures balanced_literacy toward heavier phonics weighting without foreclosing its complementarity premise); structured_literacy_reading extends the same decoding-precedence logic with additional co-equal strands and coexists with this reading as two live positions within science-of-reading-aligned camps. Each sibling story carries its own ε, beneficiary/victim structure, and stakeholder set; they are linked here for family-level contamination and coupling analysis, not collapsed into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
