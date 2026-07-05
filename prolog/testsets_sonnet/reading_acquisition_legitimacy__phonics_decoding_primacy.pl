% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Systematic Phonics as the Legitimate Reading Instruction Standard
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This story instantiates the phonics-decoding-primacy reading of the
 *   contested reading-acquisition-legitimacy kernel: reading IS decoding, and
 *   legitimate instruction is the one that makes the alphabetic code explicit
 *   and systematic rather than assuming it will be inferred from meaningful
 *   text exposure. Enacted through 'science of reading' state legislation
 *   over the past decade, this reading mandates scope-and-sequence phonics
 *   programs, decodable texts, and early screening. It coordinates a genuine
 *   cognitive-science finding (phonemic awareness and explicit code
 *   instruction reliably produce decoders where implicit exposure fails many
 *   children) with a vendor and policy-authority extraction structure
 *   (curriculum mandates create captive markets and strip teacher discretion
 *   under fidelity-monitoring regimes). This is ONE reading among several
 *   live readings of the same underlying kernel; the
 *   whole_language_meaning_primacy and balanced_literacy_integration readings
 *   are separate constraints with their own ε, beneficiaries, and victims,
 *   linked here via network.affects_constraints — they are not alternative
 *   measurements of this constraint.
 *
 * KEY AGENTS:
 *   - state_literacy_policy_offices: agenda_setter (institutional/arbitrage) — mandates and enforces the reading
 *   - phonics_curriculum_vendors: beneficiary (organized/arbitrage) — captures mandate-driven revenue
 *   - early_readers_taught_systematically: beneficiary (powerless/trapped) — genuinely served by explicit decoding instruction
 *   - whole_language_trained_teachers: payer (moderate/constrained) — bears retraining and identity cost
 *   - students_with_strong_oral_language_weak_phonemic_awareness: payer (powerless/trapped) — underserved by rigid sequencing
 *   - multilingual_learners_in_rigid_scope_and_sequence_programs: payer (powerless/trapped) — mismatched by monolingual-designed sequences
 *   - reading_researchers_cognitive_science: observer (analytical/analytical) — corroborates decoding predictor evidence while contesting scope of remedy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.32).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.44).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.32).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Systematic Phonics as the Legitimate Reading Instruction Standard").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '4be93493-67cd-47d6-ae1d-31f63e604009').
narrative_ontology:cs_kernel_codification('4be93493-67cd-47d6-ae1d-31f63e604009', distributed).
narrative_ontology:cs_authority_grounding('4be93493-67cd-47d6-ae1d-31f63e604009', expertise).
narrative_ontology:cs_interpretation_layer_present('4be93493-67cd-47d6-ae1d-31f63e604009').
narrative_ontology:cs_reading_relation('4be93493-67cd-47d6-ae1d-31f63e604009', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('4be93493-67cd-47d6-ae1d-31f63e604009', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('4be93493-67cd-47d6-ae1d-31f63e604009', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('4be93493-67cd-47d6-ae1d-31f63e604009', foundational, decoding_is_the_core_reading_acquisition_task).
narrative_ontology:cs_axiom_status(decoding_is_the_core_reading_acquisition_task, holdable).
narrative_ontology:cs_axiom_grounding('4be93493-67cd-47d6-ae1d-31f63e604009', decoding_is_the_core_reading_acquisition_task, empirically_contingent).
narrative_ontology:cs_axiom('4be93493-67cd-47d6-ae1d-31f63e604009', foundational, explicit_sequencing_outperforms_incidental_exposure).
narrative_ontology:cs_axiom_status(explicit_sequencing_outperforms_incidental_exposure, holdable).
narrative_ontology:cs_axiom_grounding('4be93493-67cd-47d6-ae1d-31f63e604009', explicit_sequencing_outperforms_incidental_exposure, empirically_contingent).
narrative_ontology:cs_reference_frame('4be93493-67cd-47d6-ae1d-31f63e604009', cognitive_science_decoding_evidence_base).
narrative_ontology:cs_drift_state('4be93493-67cd-47d6-ae1d-31f63e604009', post_science_of_reading_legislative_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4be93493-67cd-47d6-ae1d-31f63e604009', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, decodable_text_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, early_readers_taught_systematically).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, state_literacy_policy_offices).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_trained_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_strong_oral_language_weak_phonemic_awareness).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, multilingual_learners_in_rigid_scope_and_sequence_programs).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_in_scripted_curriculum_mandates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_in_scripted_curriculum_mandates).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, alphabetic_principle_centrality).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, phonemic_awareness_as_reading_predictor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce 'science of reading' legislation mandating systematic phonics scope-and-sequence programs, approve curriculum vendor lists, and require decoding-based screening assessments. They can revise mandates but bear none of the classroom-level implementation cost themselves.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, state_literacy_policy_offices, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell scripted phonics programs and decodable text sets to districts newly required by law to adopt them. Revenue flows directly from the mandate's existence; they lobby to keep the mandate specific enough to require repurchasing materials.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_vendors, beneficiary,
    organized, biographical, arbitrage, national).

% Children who lack home literacy exposure and depend on school for explicit code instruction; systematic phonics reliably gets them decoding where implicit exposure would not. They have no say in the pedagogy but genuinely benefit from its coordination function.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, early_readers_taught_systematically, beneficiary,
    powerless, biographical, trapped, local).

% Built careers and professional identity around meaning-based instruction; now required to abandon trusted methods, retrain, and follow scripted curricula under threat of evaluation penalties or licensure consequences. Exiting means leaving the profession or relocating to a jurisdiction without the mandate.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_trained_teachers, payer,
    moderate, biographical, constrained, regional).

% Rigid decoding-first sequencing can under-serve children whose comprehension outpaces their code-based drills, producing disengagement; the same systematized approach that helps decoding-deficit peers can bore or stall these students, and the mandate leaves no room for differentiated pacing.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_strong_oral_language_weak_phonemic_awareness, payer,
    powerless, biographical, trapped, local).

% Scripted phonics sequences built for monolingual English phonology can mismatch the phonemic inventories and oral vocabularies of multilingual students, and fidelity mandates leave teachers little latitude to adapt pacing or add oral-language scaffolding.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, multilingual_learners_in_rigid_scope_and_sequence_programs, payer,
    powerless, biographical, trapped, local).

% Required to deliver scripted lessons with fidelity, losing professional discretion over pacing and materials; some report relief at reduced planning burden, but administrators increasingly evaluate them on script adherence rather than reading outcomes.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_in_scripted_curriculum_mandates, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_in_scripted_curriculum_mandates, beneficiary).

% Study decoding acquisition experimentally; broadly corroborate that phonemic awareness and phonics instruction predict early decoding success, while contesting how far this evidence base extends to comprehension outcomes and to what degree of scripted rigidity is required.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_researchers_cognitive_science, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every classroom a common, empirically grounded sequence for teaching the alphabetic code, so that decoding skill does not depend on incidental exposure or teacher-specific intuition, and struggling decoders are caught early via standardized screening.
% TRANSFER_FUNCTION: Moves instructional authority and curriculum-purchasing decisions from individual teachers and districts to state policy offices and their approved vendor lists; moves revenue from district budgets to phonics curriculum and assessment publishers; moves professional autonomy from classroom teachers to script authors.
% ABSENT_VOICES: Bilingual education specialists and teachers of English learners are largely absent from state phonics mandate drafting committees; they would argue rigid monolingual scope-and-sequence design underserves students whose phonology and vocabulary differ from the assumed norm.
% DISAPPEARANCE_RATIONALE: Beneficiary seats (policy offices, vendors, decoding-deficit early readers) would say the world rearranges badly — instruction would revert to inconsistent, exposure-dependent methods and decoding gaps would widen. Payer seats (deskilled teachers, multilingual learners, comprehension-strong low-phonemic-awareness students) would say the classroom-level world barely changes for them except that professional discretion returns. The dispute is real, not resolvable from this seat alone.
% FOUNDING_PROBLEM: Whole-language and balanced-literacy instruction left a large minority of children, especially those without strong home literacy exposure or with dyslexia-type profiles, unable to decode print reliably; national reading proficiency scores stagnated for decades under those approaches.
% FOUNDING_PROBLEM_CORROBORATION: Independent cognitive science research (not funded by curriculum vendors) on phonemic awareness as a predictor of decoding success corroborates that the founding problem — inconsistent decoding acquisition under implicit-exposure models — is real and persists in NAEP and international reading assessment data; this corroboration comes from academic reading researchers outside the vendor and policy-office beneficiary set, though the same researchers dispute how far the phonics-primacy remedy should extend into comprehension pedagogy.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).
:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.32 at interval end) and rising slowly: the underlying decoding-instruction function is empirically real, so extraction stays well below tangled-rope-heavy territory, but the mandate structure increasingly channels district spending toward specific vendor-approved materials and away from teacher discretion, which is the extractive layer riding on the coordination function. Suppression is moderate (0.44) and grew faster than extraction as fidelity-monitoring and evaluation-linked compliance regimes matured over the interval — teachers face real professional and employment consequences for departing from the script, which is a suppression mechanism distinct from the extraction of resources. Theater is low (0.18): most of the activity is genuine instructional practice, not performance, though script-fidelity audits contribute a growing theatrical component. accessibility_collapse (0.58) reflects that once a state mandate is enacted, alternative pedagogies become practically inaccessible to individual teachers even though they remain intellectually available and contested in the literature — this is a constructed, policy-driven collapse, not a natural-law one. Resistance (0.5) reflects substantial pushback from displaced whole-language-trained educators and bilingual-education advocates, which is why this cannot be scored as a mountain: real alternatives are actively suppressed, not merely unavailable.
 *
 * DIRECTIONALITY LOGIC:
 *   State literacy policy offices and phonics curriculum vendors sit near the full-beneficiary end: they set the terms and capture, respectively, policy control and market revenue from the mandate's existence. Early readers who lack home literacy exposure are also genuine beneficiaries — the coordination function (explicit code instruction) reliably serves them — despite having no power or exit in the arrangement; this is a case where powerlessness and directionality-as-beneficiary coexist, which the derivation handles correctly since beneficiary status is structural, not power-conferred. Whole-language-trained teachers, comprehension-strong/phonemic-awareness-weak students, and multilingual learners in rigid programs sit near the target end: costs (deskilling, disengagement, phonological mismatch) flow to them through the same mandate structure that produces the coordination benefit for the first group. Teachers in scripted-curriculum mandates are dual-positioned (payer of autonomy, incidental beneficiary of reduced planning burden), which is why they carry a secondary_role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inconsistent decoding acquisition under implicit-exposure pedagogy — is corroborated as still live by cognitive-science research outside the immediate beneficiary set (vendors, policy offices), which is what prevents this from being scored as pure mandatrophy or a snare wearing a coordination mask. But the tangled_rope classification is warranted precisely because a real, still-live coordination function (systematic phonics genuinely helps decoding-deficit children) is now bundled with an actively enforced extraction layer (vendor capture, fidelity-monitoring suppression of teacher discretion, rigid sequencing that mismatches some learner populations) that exceeds what the coordination function alone requires. Reclassifying this as a simple rope would erase the victims; reclassifying it as a pure snare would erase the corroborated cognitive-science coordination function. Tangled rope holds both facts open simultaneously, which is the correct read.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Is the disagreement between phonics_decoding_primacy and its sibling readings located in the empirical evidence about how children learn to decode, or in the normative question of what reading education is FOR (skill acquisition vs. meaning-making vs. equity-first remediation)?',
    'Separate the empirical claim (phonemic awareness instruction improves decoding accuracy — well-corroborated) from the normative claim (decoding accuracy should be the primary early-instruction target, over comprehension-building or authentic-text engagement — contested). A sibling reading could accept the empirical finding and still reject the normative primacy claim.',
    'If the disagreement is purely normative, no amount of further decoding research resolves the kernel contest, and treating structured_literacy_remediation and phonics_decoding_primacy as convergent (both mandate explicit phonics) obscures a real difference in what problem each is centered on solving (universal instruction design vs. serving the most vulnerable first).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel contest is empirical or normative in character.').

omega_variable(
    false_summit_natural_reading_process,
    'Is ''the alphabetic principle'' a natural-law-like cognitive fact independent of instructional politics, or is treating decoding-primacy as the singular legitimate reading of reading acquisition itself a constructed framing that benefits vendors and policy offices who profit from mandate specificity?',
    'Track whether decoding-focused instruction outcomes hold up in contexts with radically different curriculum vendor markets and mandate enforcement structures (e.g., jurisdictions with strong phonics emphasis but no commercial scripted-program requirement); divergence would indicate the extraction is separable from the cognitive science.',
    'If separable, current mandate specificity (naming particular vendor-approved programs) is closer to pure rent extraction riding on a real cognitive finding, strengthening the tangled_rope read over a rope read; if inseparable, the vendor lock-in may be a necessary implementation cost of fidelity at scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_reading_process, empirical, 'Whether decoding-primacy legitimacy is naturalized cognitive science or partly constructed market/policy framing.').

omega_variable(
    multilingual_mismatch_severity,
    'How severe and how widespread is the phonological mismatch between monolingual-English-designed phonics sequences and multilingual learners'' actual language profiles?',
    'Comparative outcome data for multilingual learners under rigid-fidelity phonics mandates versus adapted or bilingual phonics sequencing.',
    'If mismatch is severe and widespread, the victim classification for multilingual learners strengthens and argues for carve-outs in mandate fidelity requirements; if mild, the coordination function may already adequately serve this population.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilingual_mismatch_severity, empirical, 'Uncertainty about the scale of harm to multilingual learners under rigid mandate fidelity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.08).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 4, 0.1).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 8, 0.13).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 12, 0.15).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 16, 0.16).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 20, 0.17).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 24, 0.18).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(read_be_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 12, 0.29).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 24, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(read_su_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one of (at least) four readings of the reading_acquisition_legitimacy kernel. phonics_decoding_primacy and structured_literacy_remediation share substantial surface overlap (both mandate explicit, systematic phonics) but differ in founding orientation: this reading centers universal instructional design around the alphabetic principle as the core reading-acquisition fact, while structured_literacy_remediation centers the needs of the most vulnerable learners as the design constraint, which produces different beneficiary/victim weighting even where instructional methods converge. whole_language_meaning_primacy and balanced_literacy_integration are downstream-affected in that phonics_decoding_primacy's political and legislative success has directly reduced their institutional legitimacy and resource access (state mandates displacing their curricula), which is the influences-type structural pressure this reading exerts on siblings without logically foreclosing them as live positions among some practitioners and researchers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
