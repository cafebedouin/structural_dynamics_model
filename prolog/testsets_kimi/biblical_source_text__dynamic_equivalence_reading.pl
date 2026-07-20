% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Bible Translation Priority
 *   domain: biblical studies/translation theory/religious authority
 *
 * SUMMARY:
 *   A dominant methodology in contemporary Bible translation holds that
 *   communicative effectiveness in the receptor language is the primary goal
 *   of translation, with formal fidelity to source-language structure
 *   subordinated to intelligibility and pastoral mission. This reading of the
 *   biblical source text is institutionalized in major Bible societies and
 *   missionary training programs. It is presented as a scientific advancement
 *   in linguistics, but systematically sacrifices the morphological and
 *   syntactic transparency required for scholarly word study. The constraint
 *   coordinates massive global scripture access while extracting epistemic
 *   precision from the scholarly community.
 *
 * KEY AGENTS:
 *   - translation_institutions (institutional/arbitrage) â administer the constraint and capture epistemic control and resource flows
 *   - lay_readers (moderate/mobile) â receive intelligible vernacular scripture as beneficiaries
 *   - missionary_contexts (organized/mobile) â deploy translated texts for evangelization as beneficiaries
 *   - word_study_scholars (moderate/constrained) â bear the cost of lost source-text precision as victims
 *   - formal_equivalence_advocates (organized/constrained) â excluded from institutional standard-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.45).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.55).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Bible Translation Priority").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "biblical studies/translation theory/religious authority").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36').
narrative_ontology:cs_kernel_codification('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', fixed_text).
narrative_ontology:cs_authority_grounding('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', expertise).
narrative_ontology:cs_interpretation_layer_present('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36').
narrative_ontology:cs_reading_relation('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', biblical_source_text__critical_reconstructive_reading, coexists_with).
narrative_ontology:cs_axiom('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', foundational, communicative_effectiveness_primary).
narrative_ontology:cs_axiom_status(communicative_effectiveness_primary, holdable).
narrative_ontology:cs_axiom_grounding('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', communicative_effectiveness_primary, instrumental).
narrative_ontology:cs_axiom('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', foundational, structural_fidelity_subordinate).
narrative_ontology:cs_axiom_status(structural_fidelity_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', structural_fidelity_subordinate, instrumental).
narrative_ontology:cs_reference_frame('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', receptor_oriented_translation_framework).
narrative_ontology:cs_drift_state('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4f29e3a7-a2d3-4f1d-b6e8-eba04d07ac36', '').
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

% Set translation philosophy and quality standards for Bible translation projects worldwide; train translators, fund projects, and certify completed translations. They collect donations and missionary resources based on the output of translation pipelines organized around communicative-effectiveness metrics.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Read Bible translations in their native languages without training in biblical languages; depend on the translation committee's interpretive choices for access to the text. They experience the text as transparent and intelligible.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_readers, beneficiary,
    moderate, biographical, mobile, global).

% Use translated scriptures for evangelism, discipleship, and church planting in cross-cultural settings. The translation approach determines how quickly and naturally biblical content can be presented in new languages.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_contexts, beneficiary,
    organized, generational, mobile, global).

% Conduct lexical, syntactical, and theological research that depends on precise mapping between the translation and the source text's morphological and syntactic structures. Dynamic equivalence obscures these structures, forcing them to bypass vernacular translations or reconstruct source features independently.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, word_study_scholars, payer,
    moderate, biographical, constrained, global).

% Promote translation methodologies that preserve source-language form; they are often sidelined in institutional committees and funding bodies that prioritize functional outcomes over formal correspondence.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, translation_institutions).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables cross-lingual communication of biblical content to receptor cultures without requiring extensive training in source languages; solves the problem of unintelligibility when formal correspondence produces awkward or meaningless constructions.
% TRANSFER_FUNCTION: Moves interpretive authority from source-text morphology and syntax to the translation committee's linguistic and cultural analysis; transfers precision from scholarly word-study access to pastoral communicative effectiveness.
% ABSENT_VOICES: Source-language experts and formal-equivalence scholars who would demand morphological transparency are underrepresented on translation committees prioritizing receptor response; readers of source languages in the Global South who could collaborate in text-critical work are bypassed by top-down translation agency workflows.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, missionary translation strategy would lose its central methodological justification, lay readers would face more linguistically foreign texts, and the current institutional pipeline of Bible translation would stall and reorganize around alternative theories.
% FOUNDING_PROBLEM: The Bible was inaccessible to common people because translations preserved source-language structures that were meaningless or misleading in receptor languages, and because only educated elites could access the original languages.
% FOUNDING_PROBLEM_CORROBORATION: Missionary organizations and Bible societies attest the problem remains live in oral cultures. Formal equivalence scholars and source-language theologians attest the founding problem was partially solved but the arrangement now creates new epistemic dependencies; independent translation consultants outside the beneficiary circle note the trade-off is real but dispute whether the current balance is correct.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).
:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint genuinely delivers intelligibility while systematically sacrificing morphological precision. Suppression (0.55) reflects institutional standard-setting that marginalizes formal-equivalence alternatives in missionary contexts. Theater ratio (0.30) captures the performative use of linguistic science to justify interpretive choices that are partly pastoral and partly expedient. Accessibility collapse (0.60) is high because once a vernacular community receives a dynamic-equivalence translation, the alternative (source-language study or formal-equivalence comparison) requires resources and training that collapse in accessibility for most users. Resistance (0.50) reflects ongoing scholarly and ecclesial pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the lay reader's seat, the constraint is experienced as transparent access to sacred text â a rope-like coordination. From the word_study_scholar's seat, the same constraint operates as an opacity layer that severs the vernacular text from the source, forcing expensive reconstruction. The institution experiences it as a productive methodology. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay readers and missionary contexts are structural beneficiaries (gaining intelligible access â d near the beneficiary end). Word_study_scholars are structural victims (losing precision â d near the target end). Translation institutions sit ambiguously: they administer the constraint and capture epistemic control, but their strong exit options pull their derived d toward the beneficiary end despite their agenda-setting role. Formal_equivalence_advocates are excluded from the arrangement and bear the cost of marginalization.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this constraint could be misread as either pure coordination (a rope serving the pastoral mission) or pure extraction (a snare against scholarly precision). The classification captures that both are structurally present: the genuine coordination of cross-lingual communication is inextricable from the asymmetric extraction of scholarly access and institutional control over meaning. Mandatrophy would occur if the founding problem â genuine unintelligibility â were fully solved but the constraint persisted purely as institutional habit; the measurements show stable rather than rising theater, suggesting the coordination function remains partly live, preventing piton descent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precision_loss_mechanism,
    'Is the loss of source-text morphological precision an unavoidable coordination cost of cross-lingual communication, or is it extractive overhead that benefits institutional translation throughput?',
    'Comparative analysis of translation projects that attempted maximal formal correspondence alongside intelligibility, measuring scholarly reconstruction costs.',
    'If unavoidable cost, extractiveness metric should be adjusted downward; if extractive overhead, the constraint leans toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precision_loss_mechanism, conceptual, 'Unavoidable cost versus extractive overhead of precision loss').

omega_variable(
    linguistic_empirical_status,
    'Does the dynamic-equivalence framework rest on empirically valid mid-20th-century linguistic theory, or has its scientific basis been substantially challenged by subsequent translation studies and cognitive linguistics?',
    'Systematic review of empirical validation studies for equivalent-effect claims in Bible translation outcomes.',
    'If the empirical basis is weak, the coordination story is cover for institutional practice, pushing classification toward snare; if strong, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_empirical_status, empirical, 'Empirical validity of the linguistic theory underlying dynamic equivalence').

omega_variable(
    kernel_transmission_or_departure,
    'Does dynamic equivalence''s subordination of structural fidelity constitute a faithful transmission of the biblical_source_text kernel, or does it create a functionally independent text that requires its own legitimating authority?',
    'Theological and hermeneutical analysis of whether meaning can be severed from source structure without creating a new kernel.',
    'If independent, the constraint is not a reading of the original kernel but a new commitment system; if transmission, the reading relation to formal_equivalence is one of genuine sibling contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_transmission_or_departure, conceptual, 'Whether dynamic equivalence transmits the kernel or departs from it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% The biblical_source_text kernel decomposes into at least three structurally distinct readings: formal_equivalence_reading (privileges source structure), dynamic_equivalence_reading (privileges receptor communication), and critical_reconstructive_reading (privileges historical-textual recovery). Each reading instantiates a different constraint with different beneficiary/victim structures and different epsilon values. They compete for institutional authority and translation-policy dominance rather than causing one another, but they form a constraint family under the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
