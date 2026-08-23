% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Classical Latin Purity Standard / Medieval Latin Corruption Thesis
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The discontinuity reading of the 'correct Latin' kernel holds that
 *   Classical Latin and Medieval Latin are distinct systems separated by a
 *   structural break. Medieval forms are not natural evolution but
 *   corruptions of the Classical ideal; reconstruction is symbolic
 *   reoccupation — recovering the lost Classical structure from the symbolic
 *   traces in texts. This reading underwrites the editorial practice of
 *   emending Medieval manuscripts toward Classical norms, the pedagogical
 *   sequence that treats Medieval Latin as degenerate, and the disciplinary
 *   boundary between Classical Philology and Medieval Studies. The constraint
 *   coordinates scholarly practice (genuine coordination: shared standards,
 *   reproducible editions) while extracting compliance from Medieval Latin
 *   scholars and excluding alternative frameworks (asymmetric extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.65).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.55).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Classical Latin Purity Standard / Medieval Latin Corruption Thesis").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '3509b2a5-0ea9-4127-9d97-4aa41b06a44d').
narrative_ontology:cs_kernel_codification('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', fixed_text).
narrative_ontology:cs_authority_grounding('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', lineage).
narrative_ontology:cs_interpretation_layer_present('3509b2a5-0ea9-4127-9d97-4aa41b06a44d').
narrative_ontology:cs_reading_relation('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', foundational, classical_latin_as_fixed_standard).
narrative_ontology:cs_axiom_status(classical_latin_as_fixed_standard, holdable).
narrative_ontology:cs_axiom_grounding('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', classical_latin_as_fixed_standard, conventional).
narrative_ontology:cs_axiom('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', foundational, medieval_forms_as_corruption).
narrative_ontology:cs_axiom_status(medieval_forms_as_corruption, holdable).
narrative_ontology:cs_axiom_grounding('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', medieval_forms_as_corruption, empirically_contingent).
narrative_ontology:cs_axiom('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', foundational, reconstruction_as_symbolic_reoccupation).
narrative_ontology:cs_axiom_status(reconstruction_as_symbolic_reoccupation, holdable).
narrative_ontology:cs_axiom_grounding('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', reconstruction_as_symbolic_reoccupation, conventional).
narrative_ontology:cs_reference_frame('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', classical_textual_ideal).
narrative_ontology:cs_drift_state('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', contemporary_philology, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3509b2a5-0ea9-4127-9d97-4aa41b06a44d', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, textual_editors).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classics_departments).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_latin_scholars).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, alternative_methodology_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, students).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, classical_latin_as_standard).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, textual_reconstruction_as_recovery).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, manuscript_corruption_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the disciplinary standards for Latin textual criticism, edit critical editions, control major journals and conference programs. Their authority derives from the claim that Classical Latin is the only stable, recoverable standard. They benefit from the framework's institutional centrality but are constrained by the need to maintain textual rigor.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_philologists, agenda_setter,
    powerful, generational, constrained, global).

% Produce the critical editions that instantiate the purity standard. Their editorial choices (emendation, normalization, apparatus criteria) enact the discontinuity thesis. They hold gatekeeping power over which readings enter the scholarly record, but their authority depends on adherence to the shared methodological framework.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, textual_editors, agenda_setter,
    institutional, generational, constrained, global).

% Structure curricula, hiring, and funding around the Classical Latin standard. The discontinuity thesis justifies a clean pedagogical sequence (Classical → Medieval as decline/corruption) and protects the disciplinary boundary against Medieval Studies. They benefit from stable enrollments and clear identity but could reorganize if the standard shifted.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classics_departments, beneficiary,
    institutional, generational, mobile, national).

% Study Medieval Latin texts but must frame their work as 'correction' of corrupt transmissions or as derivative of Classical norms. Their field is structurally marginalized: journals, grants, and positions favor Classical philology. They can publish within the framework but cannot easily challenge its foundational categories without professional penalty.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_latin_scholars, payer,
    moderate, biographical, constrained, global).

% Advocate for sociolinguistic, variationist, or continuum-based approaches to Latin (e.g., Romance philologists, Neo-Latin scholars, historical sociolinguists). Their frameworks treat Medieval Latin as natural evolution, not corruption. They are excluded from core Classical venues and must publish in peripheral or interdisciplinary outlets.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, alternative_methodology_proponents, excluded,
    moderate, biographical, trapped, global).

% Use reconstructed Classical forms as data for historical linguistics (sound change, syntax, typology). They benefit from the stability the standard provides but do not enforce it. Their analytical seat lets them see the constraint's structure without being bound by its disciplinary politics.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, historians_of_language, observer,
    organized, biographical, analytical, global).

% Learn Latin through the discontinuity framework: Classical grammar as the norm, Medieval texts as 'late' or 'corrupt' readings. They bear the pedagogical cost of a simplified narrative that obscures linguistic continuity. Exit requires leaving the educational system or seeking alternative instruction.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, students, payer,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for Latin textual criticism and pedagogy, enabling reproducible critical editions, stable curricula, and a common reference point across generations of scholars.
% TRANSFER_FUNCTION: Moves epistemic authority, editorial control, journal space, hiring lines, and grant funding from Medieval Latin studies to Classical philology. Medieval forms are framed as 'corrupt' requiring emendation toward a Classical ideal, transferring interpretive labor onto Medieval specialists who must justify their texts against the standard.
% ABSENT_VOICES: Medieval Latin specialists who see continuity with Classical Latin; Romance philologists who view Medieval Latin as early Romance; Neo-Latin scholars working in the unbroken Latin continuum; historical sociolinguists who treat language change as natural rather than degenerative. These voices are structurally excluded from core Classical venues and disciplinary governance.
% DISAPPEARANCE_RATIONALE: If the purity standard vanished, Medieval Latin would be studied as continuous evolution rather than corruption; critical editions would present Medieval texts on their own terms without emendation toward Classical norms; curricula would reorganize around variation and change; the Classical/Philology vs. Medieval Studies boundary would dissolve or radically shift.
% FOUNDING_PROBLEM: 19th-century philology needed a stable, reconstructible object of study — 'the Classical language' — and a method to recover it from manuscript traditions that showed extensive Medieval interference. The discontinuity thesis provided that object and method.
% FOUNDING_PROBLEM_CORROBORATION: Historians of philology (Sandys, Pfeiffer, Reynolds & Wilson) attest the 19th-century disciplinary need for a fixed standard from outside the Classical philology beneficiary set. Medieval Latin scholars (e.g., Mantello & Rigg, Ziolkowski) contest that the problem was ever fully solved, arguing the 'corruption' narrative obscures more than it reveals.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the transfer of authority and resources from Medieval to Classical studies. Suppression (0.55) captures the active marginalization of continuity frameworks in journals, hiring, and curricula. Theater ratio (0.35) acknowledges genuine coordination value (stable editions, shared pedagogy) while noting the ideological framing inflates the standard's necessity. Accessibility collapse (0.55) and resistance (0.55) reflect that alternatives exist (Romance philology, sociolinguistics, Neo-Latin studies) but face structural barriers to entry in core venues. The metrics are authored at interval end (200 = present); the measurement series show extraction and theater rising as the standard hardened through 19th-20th century institutionalization.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (philologists, editors), the constraint appears as genuine coordination: a necessary standard that makes textual criticism possible. From the payer seats (Medieval scholars, students), the same structure operates as enforced extraction: their materials are deemed corrupt, their frameworks excluded, their labor directed toward a standard they did not choose. The engine computes this divergence from the structural data; the authored claim (tangled_rope) asserts both functions are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and textual editors are structural beneficiaries (d near 0.0): they collect epistemic authority, editorial control, and disciplinary centrality. Medieval Latin scholars are targets (d near 1.0): they bear the cost of framing their work as correction, face publication barriers, and lack gatekeeping power. Classics departments sit near beneficiary (d ~0.2): they gain stable identity and curricula but could adapt. Alternative methodology proponents are excluded (d undefined): their exclusion is the enforcement mechanism. Students are trapped payers (d ~0.9): they absorb the framework with no exit. Historians of language are analytical observers (d = 0.5): they use the standard without enforcing it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (19th-century need for a stable reconstructible object) is contested: historians of philology corroborate it was real; Medievalists argue it was never fully solved and the solution created new distortions. The constraint persists despite the founding problem's contested status — a classic mandatrophy signal. The theater ratio rise (0.2→0.35) tracks the displacement of the original coordination function by the extraction function: the standard now maintains itself more than it solves the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discontinuity_reality,
    'Is the Classical/Medieval Latin break a genuine structural discontinuity in the language system, or a constructed boundary imposed by 19th-century philological methodology?',
    'Comparative linguistic analysis of syntactic, morphological, and lexical change rates across the 3rd-8th centuries; examination of whether Medieval ''errors'' follow systematic change patterns.',
    'If the break is constructed, the constraint''s extraction is ideological (enforcing a false standard); if real, part of the measured extraction is the genuine cost of recovering a lost system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_reality, empirical, 'Whether the discontinuity thesis describes linguistic reality or philological ideology.').

omega_variable(
    corruption_vs_evolution,
    'Are Medieval Latin ''solecisms'' and ''barbarisms'' genuine corruptions of a stable system, or the visible trace of natural language change?',
    'Corpus analysis of Medieval texts against Classical norms; sociolinguistic modeling of variation and change in diglossic situations.',
    'If forms are natural evolution, the ''correction'' editorial practice is extraction; if corruptions, correction is coordination. This directly bears on the tangled_rope vs. rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corruption_vs_evolution, conceptual, 'Whether the normative judgment ''corruption'' maps to a linguistic fact.').

omega_variable(
    kernel_reading_structure,
    'How does this reading''s structural relationship to the correct_latin_kernel and its sibling readings shape the constraint''s classification?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, enforcement mechanisms, and drift states. The engine computes per-reading types from structural data; the kernel frame predicts divergence.',
    'If the continuity reading computes as rope (low extraction) and this reading as tangled_rope, the kernel frame reveals how the same commitment (correct Latin) generates different constraint structures depending on reading. This validates the kernel/reading decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Committer-frame omega: this constraint is one reading of correct_latin_kernel; sibling readings are continuity_reading and hybrid_reading; structural delta is Medieval-forms-as-corruption with reconstruction as symbolic reoccupation.').

omega_variable(
    interpretive_layer_stability,
    'Does the textual criticism interpretation layer genuinely absorb drift (stabilizing the kernel) or does it actively reconstruct the kernel to serve disciplinary interests?',
    'Historical analysis of editorial practice changes (Lachmann, Bédier, Greg, modern digital editions); whether emendation criteria have tightened or loosened over time relative to manuscript evidence.',
    'If the layer reconstructs the kernel, the constraint''s authority_grounding=''extraction'' not ''lineage''; this changes the CS pattern classification and the drift_state interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_layer_stability, empirical, 'Whether the interpretation layer buffers or generates drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t50, correct_latin_kernel__discontinuity_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement_basis(corr_tr_t50, observed).
narrative_ontology:measurement(corr_tr_t100, correct_latin_kernel__discontinuity_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(corr_tr_t100, observed).
narrative_ontology:measurement(corr_tr_t150, correct_latin_kernel__discontinuity_reading, theater_ratio, 150, 0.33).
narrative_ontology:measurement_basis(corr_tr_t150, observed).
narrative_ontology:measurement(corr_tr_t200, correct_latin_kernel__discontinuity_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement_basis(corr_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t50, correct_latin_kernel__discontinuity_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(corr_be_t50, observed).
narrative_ontology:measurement(corr_be_t100, correct_latin_kernel__discontinuity_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement_basis(corr_be_t100, observed).
narrative_ontology:measurement(corr_be_t150, correct_latin_kernel__discontinuity_reading, base_extractiveness, 150, 0.65).
narrative_ontology:measurement_basis(corr_be_t150, observed).
narrative_ontology:measurement(corr_be_t200, correct_latin_kernel__discontinuity_reading, base_extractiveness, 200, 0.65).
narrative_ontology:measurement_basis(corr_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t50, correct_latin_kernel__discontinuity_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement_basis(corr_su_t50, observed).
narrative_ontology:measurement(corr_su_t100, correct_latin_kernel__discontinuity_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement_basis(corr_su_t100, observed).
narrative_ontology:measurement(corr_su_t150, correct_latin_kernel__discontinuity_reading, suppression_requirement, 150, 0.55).
narrative_ontology:measurement_basis(corr_su_t150, observed).
narrative_ontology:measurement(corr_su_t200, correct_latin_kernel__discontinuity_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement_basis(corr_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, medieval_latin_marginalization).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, textual_criticism_standards).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, neo_latin_exclusion).

% DUAL FORMULATION NOTE:
% This constraint is the discontinuity_reading of the correct_latin_kernel. The continuity_reading and hybrid_reading are sibling constraints. All three share the kernel commitment to a correct Latin standard but differ on the Medieval relationship. This reading's ε (0.65) is substantially higher than the continuity reading's expected ε (~0.2) because it treats Medieval forms as corruptions requiring active correction, creating extraction where the continuity reading sees only coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__discontinuity_reading, institutional, 0.15).
constraint_indexing:directionality_override(correct_latin_kernel__discontinuity_reading, powerful, 0.2).
constraint_indexing:directionality_override(correct_latin_kernel__discontinuity_reading, moderate, 0.75).
constraint_indexing:directionality_override(correct_latin_kernel__discontinuity_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
