% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__dynamic_equivalence_reading, []).

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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Primacy in Biblical Translation
 *   domain: religious/biblical_studies/translation_theory
 *
 * SUMMARY:
 *   The dynamic equivalence reading of the biblical source text kernel treats
 *   the source not as a stable linguistic object to be mapped formally, but
 *   as a communicative event whose function must be replicated in the
 *   receptor culture. Institutionalized through Bible translation agencies
 *   since the mid-twentieth century, this reading prioritizes
 *   intelligibility, pastoral impact, and reader response over morphological
 *   precision. It generates genuine coordination for lay readers and
 *   missionaries while extracting scholarly precision from word-study
 *   researchers and formal-equivalence traditions. The constraint is actively
 *   enforced through consultant checks, funding criteria, and training
 *   curricula that marginalize formal correspondence alternatives. This is a
 *   kernel reading: sibling readings (formal_equivalence_reading,
 *   critical_reconstructive_reading) instantiate structurally distinct
 *   constraints from the same source text.
 *
 * KEY AGENTS:
 *   - translation_agencies: Agenda-setter (institutional/generational/constrained) â administers standards and enforces dynamic equivalence through funding and publication gates.
 *   - lay_readers: Primary beneficiary (powerless/biographical/constrained) â receives intelligible vernacular scripture but loses access to source-text granularity.
 *   - missionary_contexts: Primary beneficiary (organized/generational/constrained) â gains effective evangelistic and pastoral tools.
 *   - word_study_scholars: Primary target/payer (organized/civilizational/identity_locked) â bears the cost of obscured morphological and syntactic precision.
 *   - formal_equivalence_advocates: Excluded voice (organized/generational/constrained) â structurally sidelined from mainstream translation institutions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.55).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.48).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Primacy in Biblical Translation").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/biblical_studies/translation_theory").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '92cb9eba-f54c-4efd-af4a-23610a341b4e').
narrative_ontology:cs_kernel_codification('92cb9eba-f54c-4efd-af4a-23610a341b4e', fixed_text).
narrative_ontology:cs_authority_grounding('92cb9eba-f54c-4efd-af4a-23610a341b4e', expertise).
narrative_ontology:cs_interpretation_layer_present('92cb9eba-f54c-4efd-af4a-23610a341b4e').
narrative_ontology:cs_reading_relation('92cb9eba-f54c-4efd-af4a-23610a341b4e', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('92cb9eba-f54c-4efd-af4a-23610a341b4e', biblical_source_text__critical_reconstructive_reading, coexists_with).
narrative_ontology:cs_axiom('92cb9eba-f54c-4efd-af4a-23610a341b4e', foundational, communicative_intent_supersedes_formal_structure).
narrative_ontology:cs_axiom_status(communicative_intent_supersedes_formal_structure, holdable).
narrative_ontology:cs_axiom_grounding('92cb9eba-f54c-4efd-af4a-23610a341b4e', communicative_intent_supersedes_formal_structure, instrumental).
narrative_ontology:cs_axiom('92cb9eba-f54c-4efd-af4a-23610a341b4e', foundational, reader_competence_determines_translation_shape).
narrative_ontology:cs_axiom_status(reader_competence_determines_translation_shape, holdable).
narrative_ontology:cs_axiom_grounding('92cb9eba-f54c-4efd-af4a-23610a341b4e', reader_competence_determines_translation_shape, empirically_contingent).
narrative_ontology:cs_reference_frame('92cb9eba-f54c-4efd-af4a-23610a341b4e', functional_communicative_event).
narrative_ontology:cs_drift_state('92cb9eba-f54c-4efd-af4a-23610a341b4e', contemporary_evangelical_resurgence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('92cb9eba-f54c-4efd-af4a-23610a341b4e', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_contexts).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, word_study_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer translation standards for Bible societies and missionary organizations, enforcing dynamic equivalence through consultant approval processes, training curricula, and publication funding gates. They determine which translation methodologies receive institutional support and which are marginalized.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Receive vernacular scripture translations optimized for immediate comprehension and cultural resonance. They benefit from accessibility but lack training to evaluate what source-text structural features have been removed or reshaped in the process.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_readers, beneficiary,
    powerless, biographical, constrained, local).

% Rely on dynamically equivalent translations to facilitate evangelism, discipleship, and church planting across linguistic boundaries. The methodology reduces the time and theological training required for scripture engagement in receptor cultures.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_contexts, beneficiary,
    organized, generational, constrained, global).

% Depend on morphological, syntactic, and lexical transparency for philological research, theological argumentation, and inter-textual biblical analysis. Dynamic equivalence obscures the granular source-text features their disciplines require, forcing reliance on original languages or formal-equivalent tools that are increasingly institutionally sidelined.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, word_study_scholars, payer,
    organized, civilizational, identity_locked, global).

% Theological traditions and scholars who regard formal correspondence to source language structures as essential for doctrinal precision and ecclesial stability. They are structurally excluded from mainstream Bible society translation committees, funding streams, and consultant networks.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, diffuse).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables cross-linguistic and cross-cultural communication of biblical content to receptor audiences who lack source-language competence or extensive theological training, solving the problem of intelligibility and communicative impact across linguistic distance.
% TRANSFER_FUNCTION: Moves epistemic authority and textual control from source-language scholars and formal structural fidelity to translators, linguists, and missionary practitioners who determine the appropriate dynamic equivalent in the target language and culture.
% ABSENT_VOICES: Source-text philologists, formal-equivalence traditionalists, and postcolonial critics who challenge whether communicative effectiveness encodes Western missionary assumptions rather than neutral linguistic science; they are largely excluded from Bible society translation committees and funding bodies.
% DISAPPEARANCE_RATIONALE: If dynamic equivalence as an institutional priority vanished overnight, Bible translation practices would fragment: formal-equivalence projects would regain funding and institutional support, lay readers would encounter more opaque but structurally transparent texts, missionary methodologies would slow to accommodate linguistic training, and the scholarly ecosystem around word studies would reorganize around translations preserving morphological precision.
% FOUNDING_PROBLEM: The Protestant missionary expansion of the nineteenth and twentieth centuries encountered receptor languages and cultures for which literal translation produced incomprehensible, misleading, or theologically distorted scripture, creating a need for translation theory that prioritized comprehension and communicative response over formal linguistic correspondence.
% FOUNDING_PROBLEM_CORROBORATION: Bible translation agencies and linguistic missionaries attest the problem remains live in unreached language groups. Postcolonial scholars and formal-equivalence advocates attest the problem has been co-opted to justify translatorial control and theological simplification; independent linguists outside missionary institutions note that the problem is often overstated to maintain agency relevance and funding.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint genuinely solves cross-linguistic communication problems while systematically subordinating source-text transparency. Suppression (0.48) reflects institutional gatekeeping rather than physical coercion â formal-equivalence proposals are defunded and scholars are excluded from committees. Theater ratio (0.32) captures the performative missionary rhetoric about making the Word accessible that obscures methodological control. Accessibility collapse (0.58) is moderate: alternatives exist but are institutionally starved. Resistance (0.38) is moderate â scholarly pushback is real but institutionally muted. The measurement series shows extraction rising as the model became dominant, then slightly moderating under recent formal-equivalence resurgence and postcolonial critique.
 *
 * PERSPECTIVAL GAP:
 *   The lay reader and missionary seats compute toward rope-like coordination (low d, low chi): they receive intelligible texts and effective ministry tools. The word-study scholar seat computes toward snare-like extraction (high d, high chi): the same constraint removes the lexical and syntactic transparency their discipline requires. The translation agency seat sits near the agenda-setter pole with moderate d â they administer the constraint and benefit from institutional centrality, though they are not financial rentiers. The engine computes this divergence from the structural role and exit declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (lay_readers, missionary_contexts) derive low directionality: the constraint subsidizes their access to scripture. Victim declaration (word_study_scholars) derives high directionality: the constraint extracts source-text precision from their work. The agenda_setter (translation_agencies) is not declared in either beneficiary or victim arrays, so it falls back to the canonical institutional default (moderate d), reflecting that agencies both maintain and are constrained by the methodology.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling by requiring both coordination (genuine intelligibility gains) and extraction (precision loss for scholars) to be present. A pure rope reading would ignore the marginalization of formal-equivalence scholars and the institutional suppression of alternative methodologies. A pure snare reading would ignore the real communicative successes in vernacular translation. The tangled_rope classification captures that the same arrangement coordinates missionary practice while extracting scholarly access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the subordination of structural fidelity a necessary cost of cross-linguistic communication, or an epistemic extraction that conceals source-text features from readers?',
    'Comparative analysis of comprehension outcomes between formal and dynamic translations in the same language community, plus reader surveys on perceived versus actual source-text access.',
    'If dynamic equivalence hides necessary theological nuance, extraction is higher than coordination; if it merely solves genuine linguistic asymmetry, coordination is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Whether precision loss is coordination cost or extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of word-study scholars structural (funding and publication gates) or internalized (scholars adopting dynamic equivalence assumptions to remain relevant)?',
    'Track career trajectories of formal-equivalence scholars in Bible society contexts versus independent academic institutions.',
    'If internalized, effective suppression exceeds structural measure because the target carries the constraint after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    kernel_baseline_interference,
    'Does the classification of dynamic equivalence as tangled rope depend on treating formal equivalence as the implicit baseline, or would it remain extractive even under a functional-communication baseline?',
    'Compare classification under counterfactual baseline shifts â evaluate whether beneficiary and victim arrays invert when the reference frame is changed to functional communication as the default.',
    'If classification flips under baseline shift, the extraction is observer-relative; if stable, the extraction is intrinsic to the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_baseline_interference, conceptual, 'Observer relativity of extraction relative to sibling baseline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 60, 0.32).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 60, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 50, 0.47).
narrative_ontology:measurement(bibl_su_t60, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 60, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the biblical_source_text kernel. The label 'biblical source text' conflates three structurally distinct hermeneutical commitments: formal equivalence (linguistic object), dynamic equivalence (communicative event), and critical reconstruction (historical recovery). Each reading carries a distinct epsilon and stakeholder geometry, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
