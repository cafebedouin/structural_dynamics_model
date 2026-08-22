% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Correct Latin Kernel â Hybrid Reading
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The correct Latin kernel is the commitment system that grounds
 *   philological authority in a recoverable classical Latin standard. Under
 *   the hybrid reading, core morphology is treated as directly continuous
 *   from antiquity, while syntax and lexicon are regarded as corrupted or
 *   displaced, requiring layered textual reconstruction to recover the
 *   authentic kernel. This reading coordinates European philology and textual
 *   editing across the long nineteenth century, but it asymmetrically
 *   extracts epistemic authority from medieval Latinists by systematically
 *   devaluing post-classical syntactic and lexical innovation as corruption
 *   rather than evolution. The constraint is claimed as coordination (a
 *   necessary editorial framework) while the metrics describe substantial
 *   extraction through enforced reconstruction norms.
 *
 * KEY AGENTS:
 *   - classical_philologists: Primary agenda-setter (institutional/identity_locked) â defines the hybrid framework and controls the classical canon.
 *   - textual_editors: Secondary agenda-setter (organized/constrained) â enforces the layered reconstruction method in critical editions.
 *   - humanist_academies: Beneficiary (institutional/constrained) â collects prestige and resources from the classical continuity narrative.
 *   - medieval_latinists: Primary payer (moderate/constrained) â bears the epistemic cost of having their objects of study framed as degradation.
 *   - discontinuity_scholars: Excluded voice (moderate/constrained) â would advance a distinct-systems framework but is kept out of mainstream institutions.
 *   - intellectual_historians: Analytical observer (analytical/analytical) â sees the institutional structure of the kernel dispute.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.62).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.55).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Correct Latin Kernel â Hybrid Reading").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '48cf5f18-517b-441e-87b9-cfbfd384bff3').
narrative_ontology:cs_kernel_codification('48cf5f18-517b-441e-87b9-cfbfd384bff3', fixed_text).
narrative_ontology:cs_authority_grounding('48cf5f18-517b-441e-87b9-cfbfd384bff3', lineage).
narrative_ontology:cs_interpretation_layer_present('48cf5f18-517b-441e-87b9-cfbfd384bff3').
narrative_ontology:cs_reading_relation('48cf5f18-517b-441e-87b9-cfbfd384bff3', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('48cf5f18-517b-441e-87b9-cfbfd384bff3', correct_latin_kernel__discontinuity_reading, influences).
narrative_ontology:cs_axiom('48cf5f18-517b-441e-87b9-cfbfd384bff3', foundational, morphological_continuity_core).
narrative_ontology:cs_axiom_status(morphological_continuity_core, holdable).
narrative_ontology:cs_axiom_grounding('48cf5f18-517b-441e-87b9-cfbfd384bff3', morphological_continuity_core, empirically_contingent).
narrative_ontology:cs_axiom('48cf5f18-517b-441e-87b9-cfbfd384bff3', foundational, syntactic_lexical_recovery_necessity).
narrative_ontology:cs_axiom_status(syntactic_lexical_recovery_necessity, holdable).
narrative_ontology:cs_axiom_grounding('48cf5f18-517b-441e-87b9-cfbfd384bff3', syntactic_lexical_recovery_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('48cf5f18-517b-441e-87b9-cfbfd384bff3', classical_latin_authority).
narrative_ontology:cs_drift_state('48cf5f18-517b-441e-87b9-cfbfd384bff3', contemporary_historical_linguistics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('48cf5f18-517b-441e-87b9-cfbfd384bff3', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, textual_editors).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, humanist_academies).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_latinists).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, classical_supremacy_doctrine).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, layered_reconstruction_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the methodological framework for Latin textual criticism, asserting that core morphology is continuous from antiquity while syntax and lexicon require reconstruction. Their authority derives from mastery of the classical corpus and the editorial tradition. They occupy academic chairs, control critical editions, and train successors within the hybrid paradigm.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classical_philologists, agenda_setter,
    institutional, generational, identity_locked, continental).

% Produce critical editions according to the layered reconstruction method, emending medieval syntactic and lexical variants back toward putative classical originals. Their professional standing depends on fidelity to the hybrid model; departures risk rejection by the philological establishment.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, textual_editors, agenda_setter,
    organized, biographical, constrained, continental).

% Universities and academies that maintain classical Latin curricula and philological chairs. They derive prestige and enrollment from the authority of the correct Latin kernel, and their institutional identity is bound to the continuity narrative.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, humanist_academies, beneficiary,
    institutional, generational, constrained, continental).

% Study post-classical Latin texts but must justify their materials within a framework that treats medieval syntax and lexicon as corruptions of a classical kernel. Their research is consistently measured against classical norms, and positive findings are often framed as survivals or degradations rather than innovations.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_latinists, payer,
    moderate, biographical, constrained, continental).

% Would treat Classical and Medieval Latin as distinct linguistic systems warranting separate analytical frameworks. They are largely excluded from mainstream philological journals, editorial boards, and funding bodies because their framework contradicts the reconstruction imperative.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, discontinuity_scholars, excluded,
    moderate, biographical, constrained, national).

% Analyze the philological tradition as an institution, tracing how the hybrid reading emerged to mediate between humanist reverence for antiquity and the empirical discovery of medieval textual variety. They are not bound to the constraint's authority claims.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, intellectual_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified methodological framework for textual criticism and Latin pedagogy across two millennia of texts, allowing scholars to coordinate editorial practice and curriculum design around a recoverable classical kernel.
% TRANSFER_FUNCTION: Moves epistemic authority and institutional prestige from medieval syntactic and lexical innovation to classical philologists and reconstructionist editors, who control the editorial apparatus and the correct textual standard.
% ABSENT_VOICES: Vernacular medievalists and discontinuity scholars, who would treat post-classical syntactic and lexical forms as legitimate evolutionary or contact-driven developments rather than corruptions, are largely excluded from mainstream philological institutions and editorial boards.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished, critical edition methodology would fragment between pure continuity and discontinuity models, curricula would reorganize to grant medieval Latin syntactic and lexical autonomy, and the institutional prestige of the classical reconstruction apparatus would dissipate.
% FOUNDING_PROBLEM: The need to reconcile humanist reverence for classical Latin authority with the empirical reality of vast, messy post-classical textual corpora that did not conform to classical norms.
% FOUNDING_PROBLEM_CORROBORATION: Intellectual historians and some philologists outside the classical establishment attest the founding problem was partly an ideological construct to preserve classical authority; classical philologists attest it was a genuine editorial problem. No neutral corroboration exists â the dispute is structural.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the hybrid framework systematically redirects scholarly attention and institutional resources away from medieval syntactic and lexical autonomy toward classical reconstruction. Suppression (0.55) reflects the structural marginalization of discontinuity readings in journals, curricula, and funding bodies, not physical coercion. Theater ratio (0.45) captures the performative aspect of layered reconstruction â the repeated ritual of recovering a classical original that may never have existed in the form imagined. Accessibility collapse (0.60) is moderate-high: once inside the hybrid paradigm, alternative editorial methods appear methodologically naive. Resistance (0.50) reflects sustained but institutionally muted pushback from medievalists and historical linguists.
 *
 * PERSPECTIVAL GAP:
 *   From the classical philologist's seat, the constraint is a necessary methodological framework that solves the genuine problem of textual corruption across centuries. From the medieval latinist's seat, the same constraint operates as an epistemic mechanism that preemptively disqualifies their subject matter as legitimate in its own right. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and textual editors sit near the beneficiary end of directionality: the constraint subsidizes their authority and professional standing by centralizing textual legitimacy in their reconstruction apparatus. Humanist academies are diffuse beneficiaries. Medieval latinists sit near the full-target end: the constraint extracts epistemic standing from their domain by framing medieval syntax and lexicon as problems to be corrected rather than phenomena to be studied. Their exit is constrained by the structure of academic hiring and publishing.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents a simple mandatrophy mislabeling because it genuinely coordinates editorial practice: without some shared standard, critical editions of widely divergent medieval manuscripts would be incomparable and pedagogically unusable. However, the coordination function does not justify the asymmetric extraction. The framework was built to solve an editorial problem, but its persistence and enforcement structure now serve to maintain classical institutional authority beyond the point where the editorial problem required this specific solution. The Tangled Rope classification captures this duality â it is neither pure coordination (Rope) nor pure extraction (Snare), and the metrics are authored to reflect that the extraction component is substantial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_contingency,
    'Is the hybrid reading (continuous morphology, recovered syntax/lexicon) a structurally necessary description of Latin historical linguistics, or a contingent institutional frame that privileges classical authority?',
    'Comparative analysis of philological traditions outside the European classical academy (e.g., Arabic or Sanskrit textual traditions) to see if analogous hybrid readings emerge independently, or if the structure is specific to the Latin institutional context.',
    'If contingent and institution-specific, the constraint''s classification shifts toward extraction-heavy tangled_rope or snare; if independently discoverable, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_contingency, conceptual, 'Whether the hybrid reading is a natural feature of philology or a constructed authority mechanism.').

omega_variable(
    medieval_syntax_legitimacy,
    'To what extent do medieval Latin syntactic and lexical innovations represent autonomous linguistic development versus corruption of a classical kernel?',
    'Corpus linguistics and sociolinguistic analysis of medieval Latin texts compared to contemporary vernaculars, assessing whether syntactic structures are internally systematic.',
    'If autonomous, the extraction from medieval latinists is severe and the coordination function is weak cover; if genuinely corruptions, the extraction is the legitimate cost of textual recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_syntax_legitimacy, empirical, 'Empirical status of medieval Latin syntactic and lexical autonomy.').

omega_variable(
    reconstruction_layering_necessity,
    'Is layered reconstruction (textual, then linguistic) the only viable method for producing critical editions of medieval Latin texts, or does it impose an unnecessary classical filter?',
    'Editorial experiments producing parallel editions: one under hybrid reconstruction, one under discontinuity assumptions, compared for scholarly utility and fidelity to manuscript evidence.',
    'If alternative editing methods prove viable, the constraint''s accessibility collapse is lower than currently measured and its suppression of discontinuity readings is more exposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_layering_necessity, conceptual, 'Whether layered reconstruction is methodologically necessary or arbitrarily restrictive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t10, correct_latin_kernel__hybrid_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__hybrid_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(corr_tr_t30, correct_latin_kernel__hybrid_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(corr_tr_t40, correct_latin_kernel__hybrid_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(corr_tr_t50, correct_latin_kernel__hybrid_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(corr_be_t10, correct_latin_kernel__hybrid_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__hybrid_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(corr_be_t30, correct_latin_kernel__hybrid_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(corr_be_t40, correct_latin_kernel__hybrid_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(corr_be_t50, correct_latin_kernel__hybrid_reading, base_extractiveness, 50, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(correct_latin_kernel__hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, discontinuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the correct_latin_kernel family. The hybrid reading decomposes the kernel into a partial-continuity claim, structurally distinct from the full continuity and full discontinuity readings. Each reading has a different epsilon and stakeholder directionality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
