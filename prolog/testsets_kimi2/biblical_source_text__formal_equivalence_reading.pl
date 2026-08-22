% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__formal_equivalence_reading, []).

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
 *   constraint_id: biblical_source_text__formal_equivalence_reading
 *   human_readable: Formal Equivalence Bible Translation Philosophy
 *   domain: religious/biblical_studies/translation_theory
 *
 * SUMMARY:
 *   This constraint instantiates the formal_equivalence_reading of the
 *   biblical_source_text kernel: the translation philosophy that
 *   source-language structure is primary and target-language intelligibility
 *   is a subordinate responsibility of the reader's community and teachers.
 *   It is actively enforced through conservative Bible translation
 *   committees, denominational endorsement structures, and seminary
 *   curricula. Non-specialist readers bear the cost of reduced immediate
 *   comprehensibility, while hermeneutically conservative communities and
 *   scholarly gatekeepers benefit from the textual stability and authority
 *   mediation the philosophy creates. The constraint is claimed as
 *   tangled_rope because it carries a genuine coordination function (textual
 *   stability across languages) alongside asymmetric extraction (dependent
 *   lay readership).
 *
 * KEY AGENTS:
 *   - Formal equivalence translation committees (agenda_setter/institutional/constrained): set and enforce translation philosophy, receive institutional mandate.
 *   - Conservative denominational networks (beneficiary/organized/identity_locked): gain doctrinal stability and congregational authority through mediated text.
 *   - Biblical scholars gatekeepers (beneficiary/moderate/constrained): supply necessary teaching and commentary, professional status depends on textual opacity.
 *   - Lay readers (payer/powerless/identity_locked): bear the cost of foreign syntax and vocabulary, dependent on expert mediation.
 *   - Dynamic equivalence advocates (excluded/organized/constrained): structurally excluded from formal-equivalence decision forums.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, 0.74).
domain_priors:suppression_score(biblical_source_text__formal_equivalence_reading, 0.65).
domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__formal_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__formal_equivalence_reading, "Formal Equivalence Bible Translation Philosophy").
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious/biblical_studies/translation_theory").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, 'bebdec90-0c1d-4f5f-8a32-be13f9b88bec').
narrative_ontology:cs_kernel_codification('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', fixed_text).
narrative_ontology:cs_authority_grounding('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', lineage).
narrative_ontology:cs_interpretation_layer_present('bebdec90-0c1d-4f5f-8a32-be13f9b88bec').
narrative_ontology:cs_reading_relation('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', foundational, source_structure_carries_revelational_authority).
narrative_ontology:cs_axiom_status(source_structure_carries_revelational_authority, holdable).
narrative_ontology:cs_axiom_grounding('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', source_structure_carries_revelational_authority, theological).
narrative_ontology:cs_axiom('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', foundational, translator_owes_source_fidelity_not_target_fluency).
narrative_ontology:cs_axiom_status(translator_owes_source_fidelity_not_target_fluency, holdable).
narrative_ontology:cs_axiom_grounding('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', translator_owes_source_fidelity_not_target_fluency, conventional).
narrative_ontology:cs_reference_frame('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', source_text_normative_priority).
narrative_ontology:cs_drift_state('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', contemporary_evangelical_translation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bebdec90-0c1d-4f5f-8a32-be13f9b88bec', '').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, formal_equivalence_translation_committees).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, conservative_denominational_networks).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, biblical_scholars_gatekeepers).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, lay_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce translation philosophy for major Bible versions through revision committees, copyright control, and denominational endorsements. Receive funding, institutional mandate, and scholarly legitimacy from conservative networks. Switching to dynamic equivalence would mean abandoning their core institutional identity and donor base.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, formal_equivalence_translation_committees, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, formal_equivalence_translation_committees, beneficiary).

% Receive doctrinal stability and continuity from texts that resist interpretive drift. Their authority over congregations is reinforced by the necessity of expert mediation for formally equivalent scriptures. Abandoning this translation philosophy would trigger identity crises and boundary disputes.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, conservative_denominational_networks, beneficiary,
    organized, generational, identity_locked, global).

% Provide teaching, commentary, and preaching that bridges the intelligibility gap created by source-structure fidelity. Their professional status, seminary employment, and role as necessary mediators depend on the text remaining structurally foreign to lay readers.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, biblical_scholars_gatekeepers, beneficiary,
    moderate, biographical, constrained, global).

% Read Bibles whose syntax, word order, and vocabulary deviate significantly from their natural language, creating dependence on pastors, study Bibles, and scholarly notes for basic comprehension. Their religious identity is fused to communities that treat this foreignness as a mark of reverence, making exit psychologically and socially costly.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, lay_readers, payer,
    powerless, biographical, identity_locked, local).

% Promote translation philosophies that prioritize target-language intelligibility and communicative effectiveness. They are structurally excluded from formal-equivalence translation committees and marginalized in conservative denominational curricula, though their translations achieve wide adoption outside formal-equivalence strongholds.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, dynamic_equivalence_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__formal_equivalence_reading, diffuse).
narrative_ontology:fixing_cost_class(biblical_source_text__formal_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains textual stability and cross-linguistic comparability by preserving source-language syntactic structures, morphological details, and lexical forms, enabling scholarly verification and denominational continuity across languages and centuries.
% TRANSFER_FUNCTION: Moves interpretive labor and authority from the translator to the scholarly and pastoral gatekeeper: the translation preserves source-language opacity, transferring the cost of intelligibility to lay readers and the benefit of necessary mediation to scholars and teaching institutions.
% ABSENT_VOICES: Dynamic equivalence translators, oral-culture Christian communities, and non-specialist readers from low-literacy contexts are structurally excluded from translation philosophy decisions; their need for immediate intelligibility is overridden by the priority of source-structure fidelity.
% DISAPPEARANCE_RATIONALE: If formal equivalence vanished as the operative norm, Bible translation would reorganize around target-language communicative effectiveness, seminary curricula would de-emphasize source-language exegesis as a prerequisite for access, conservative denominational boundaries would soften as lay readers gained direct textual comprehension, and the professional role of the biblical scholar as necessary mediator would contract.
% FOUNDING_PROBLEM: The biblical text is linguistically and culturally distant from modern readers; early translation sought to preserve the exact wording and structure believed to carry divine authority, avoiding interpretive drift that might arise from target-language adaptation.
% FOUNDING_PROBLEM_CORROBORATION: Conservative institutions and translation committees attest the problem is still live, citing the threat of doctrinal drift. Dynamic equivalence advocates, sociolinguists, and literacy researchers from outside the benefiting parties attest that the founding problem has shifted: the contemporary problem is accessibility, and formal equivalence exacerbates it by design.
narrative_ontology:disappearance_verdict(biblical_source_text__formal_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__formal_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__formal_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__formal_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__formal_equivalence_reading, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__formal_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__formal_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74) because the intelligibility gap is systematically preserved rather than closed by the translator, creating a permanent demand for gatekeeping. Suppression (0.65) reflects the institutional exclusion of dynamic equivalence advocates from committees and curricula. Theater ratio (0.48) captures the growing gap between 'literal' branding and softened contemporary practice (e.g., 'essentially literal' translations). Accessibility collapse (0.72) is high because once a community commits to formal equivalence, alternative translation philosophies are read as doctrinal compromise. Resistance (0.55) reflects sustained advocacy for dynamic equivalence and periodic denominational disputes over Bible translation policy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as preserving divine authority and textual purity; the payer seat experiences it as an artificial barrier to direct understanding. The engine will compute low directionality for the translation committees and scholar gatekeepers, and high directionality for lay readers, producing divergent per-seat classifications. The excluded dynamic-equivalence advocates would compute as even higher directionality if they were trapped inside the constraint rather than pushed outside it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (translation committees, denominational networks, scholar gatekeepers) have low directionality because the constraint subsidizes their institutional existence, authority, and employment. Victims (lay readers) have high directionality because they pay the intelligibility cost and are identity-locked into communities that treat this cost as piety. The derivation chain produces this mapping from beneficiary/victim declarations combined with exit options: identity_locked and powerless victims sit near the full-target end, while institutional and organized beneficiaries with constrained exit sit near the full-beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a scaffold: it carries no sunset clause and is not justified as transitional. It is not a piton: the coordination function (textual stability) is still actively performed, not atrophied, though theater is rising. It is not a pure snare because the coordination function is structurally genuineâformal equivalence does produce cross-linguistic comparability that dynamic equivalence sacrifices. It is not a pure rope because the intelligibility cost is not borne symmetrically; it is offloaded onto powerless, identity-locked lay readers while scholars and institutions capture the authority benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the formal_equivalence_reading of the biblical_source_text kernel. Siblings are dynamic_equivalence_reading and critical_reconstructive_reading. Where is the structural disagreement located among these three readings?',
    'Comparative analysis of the three sibling constraints'' stakeholder arrays, epsilon values, and axiom sets.',
    'Determines whether the disagreement is empirical (what translations actually achieve), conventional (what translators ought to prioritize), or theological (what divine revelation requires of translators).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Locator for this reading within the biblical_source_text kernel.').

omega_variable(
    dynamic_equivalence_inversion,
    'If dynamic_equivalence_reading were adopted as the operative constraint, would the payer/beneficiary structure invert and would overall extraction decrease?',
    'Compare non-specialist reader directionality and effective extraction across the two reading constraints.',
    'If inversion is complete and extraction drops, formal equivalence functions as a snare relative to dynamic equivalence; if extraction merely shifts from scholar gatekeepers to pastoral gatekeepers, the kernel carries tangled-rope dynamics regardless of reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_equivalence_inversion, conceptual, 'Structural delta between formal and dynamic equivalence readings.').

omega_variable(
    naturalness_of_gatekeeping,
    'Is the requirement for expert mediation of scripture a natural consequence of divine transcendence and linguistic distance, or a constructed mechanism for maintaining institutional authority?',
    'Cross-cultural comparison of high-context versus low-context religious traditions; measurement of lay comprehension across translation philosophies.',
    'If natural, the constraint''s extraction is the necessary price of encountering a foreign divine text; if constructed, extraction is surplus to the coordination function and the constraint leans toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_gatekeeping, conceptual, 'Whether gatekeeping is natural or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 0, 53).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__formal_equivalence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bibl_tr_t26, biblical_source_text__formal_equivalence_reading, theater_ratio, 26, 0.35).
narrative_ontology:measurement(bibl_tr_t53, biblical_source_text__formal_equivalence_reading, theater_ratio, 53, 0.48).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__formal_equivalence_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(bibl_be_t26, biblical_source_text__formal_equivalence_reading, base_extractiveness, 26, 0.66).
narrative_ontology:measurement(bibl_be_t53, biblical_source_text__formal_equivalence_reading, base_extractiveness, 53, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__formal_equivalence_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bibl_su_t26, biblical_source_text__formal_equivalence_reading, suppression_requirement, 26, 0.56).
narrative_ontology:measurement(bibl_su_t53, biblical_source_text__formal_equivalence_reading, suppression_requirement, 53, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% The biblical_source_text kernel decomposes into three structurally distinct constraints: formal_equivalence_reading (source structure priority), dynamic_equivalence_reading (target intelligibility priority), and critical_reconstructive_reading (historical recovery priority). Each reading has a distinct epsilon, stakeholder structure, and beneficiary/victim distribution. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
