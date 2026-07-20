% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Catastrophe Memory Survival â Hybrid Encoding Reading
 *   domain: religious studies / collective memory / ritual practice
 *
 * SUMMARY:
 *   This constraint is the hybrid_encoding_reading of the contested kernel
 *   catastrophe_memory_survival. The kernel concerns how ritual preserves
 *   group memory across catastrophes. The sibling readings are
 *   symbol_survival_reading (ritual as pure identity continuity) and
 *   competence_transmission_reading (ritual as practical knowledge archive).
 *   This reading synthesizes both, claiming that separating the registers
 *   destroys the survival function. The decomposition follows the
 *   Îµ-invariance principle: each reading has a distinct Îµ and
 *   victim/beneficiary structure.
 *
 * KEY AGENTS:
 *   - Communities maintaining registers: Primary beneficiary (organized/regional/identity_locked) â receive continuity and survival encoding
 *   - Analysts forcing binary classification: Primary payer (moderate/global/constrained) â bear epistemic and methodological costs
 *   - Comparative ritual theorists: Analytical observer â sees the cross-cultural pattern without bearing costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Catastrophe Memory Survival â Hybrid Encoding Reading").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious studies / collective memory / ritual practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__hybrid_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '5b08e24a-adb5-4714-bbd1-f72a5f50bbcf').
narrative_ontology:cs_kernel_codification('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', distributed).
narrative_ontology:cs_authority_grounding('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', practice).
narrative_ontology:cs_interpretation_layer_present('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf').
narrative_ontology:cs_reading_relation('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', foundational, dual_register_necessary_for_survival).
narrative_ontology:cs_axiom_status(dual_register_necessary_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', dual_register_necessary_for_survival, empirically_contingent).
narrative_ontology:cs_axiom('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', foundational, binary_analytic_frame_incommensurable).
narrative_ontology:cs_axiom_status(binary_analytic_frame_incommensurable, holdable).
narrative_ontology:cs_axiom_grounding('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', binary_analytic_frame_incommensurable, instrumental).
narrative_ontology:cs_reference_frame('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', communal_practice_continuity).
narrative_ontology:cs_drift_state('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', post_literacy_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b08e24a-adb5-4714-bbd1-f72a5f50bbcf', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, communities_maintaining_registers).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, analysts_forcing_binary_classification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain ritual cycles that encode both identity-boundary markers and practical instructions for catastrophe response. They do not separate the symbolic and practical layers; the ritual is performed as a unified whole. Exit would mean ceasing to be the community, losing both the identity and the encoded survival knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, communities_maintaining_registers, beneficiary,
    organized, generational, identity_locked, regional).

% Are socialized in academic disciplines that demand classification of ritual behavior as either symbolic or functionalist/practical. When they encounter communities where the two registers are inseparable, their analytical frameworks produce systematic misreadings, requiring costly methodological reframes or generating publishable failures. They experience this as theoretical resistance from the object of study.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, analysts_forcing_binary_classification, payer,
    moderate, biographical, constrained, global).

% Document the cross-cultural pattern that catastrophe-survival rituals resist binary decomposition. They stand outside the benefit/cost structure, observing that the constraint produces seat divergence without being captured by either register.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, comparative_ritual_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preservation of actionable survival knowledge and group identity across catastrophes and generational breaks in communities without centralized literacy or archival infrastructure.
% TRANSFER_FUNCTION: Moves epistemic complexity and methodological cost onto analysts who deploy binary classification frameworks; moves continuity-of-practice benefits to the communities who maintain the ritual.
% ABSENT_VOICES: Communities that abandoned ritual practice and lost catastrophe memory; analysts capable of non-binary hybrid frameworks who are marginalized in disciplinary gatekeeping.
% DISAPPEARANCE_RATIONALE: If the dual-register constraint vanished, communities would lose the hybrid encoding that couples identity to survival knowledge, forcing reliance on either pure symbolism (non-actionable) or pure functionalism (non-memorable); analysts would regain binary clarity but lose explanatory validity.
% FOUNDING_PROBLEM: How to preserve both group identity and actionable survival instructions across catastrophes when writing is absent, institutions are fragile, and either register alone fails (symbolism lacks practicality; pure competence lacks motivational binding).
% FOUNDING_PROBLEM_CORROBORATION: Archaeologists and disaster-studies scholars outside the benefiting communities attest that ritual encoding correlates with survival outcomes; some symbolic anthropologists contest the practical register's necessity, attributing survival to external factors.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint's primary motion is coordination: coupling identity to survival knowledge so that neither atrophies. Suppression is moderate (0.45) because the ritual resists external binary parsing and is maintained by social enforcement of practice. Theater is low (0.15): the ritual is largely functional, though modernity may be increasing symbolic performance relative to practical content. Accessibility collapse is high (0.82): once the dual register is seen, pure-symbol or pure-competence readings collapse as adequate descriptions. Resistance is moderate (0.55): analysts resist the hybrid frame because it breaks their disciplinary tools, and some modernizers within communities resist the labor of maintaining both registers.
 *
 * PERSPECTIVAL GAP:
 *   The communities experience the constraint as lived necessity and identity; the analysts experience it as epistemic obstruction. The engine computes this divergence from the structural data: identical ritual behavior produces beneficiary-directionality for the communities and target-directionality for the analysts. The authored claim (tangled_rope) reflects mild but real asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Communities are declared beneficiaries with identity-locked exit, producing low directionalities (subsidized by the constraint). Analysts are declared victims with constrained exit, producing high directionalities (extracted by the constraint). The comparative theorist sits at the analytical pole with no directional stake. No overrides are needed: the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a snare (extraction is too low, and the coordination function is genuine and well-corroborated) and prevents mislabeling it as a rope (the asymmetric cost borne by analysts is structural, not incidental). If the analysts' cost were ignored, the constraint would appear as a rope; if the coordination function were ignored, it would appear as a snare. Tangled rope is the only category that respects both registers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    analyst_victimhood_validity,
    'Are analysts who force binary classification genuinely victims of extraction, or merely encountering an epistemically complex object?',
    'Measure disciplinary citation and career penalties for scholars publishing binary-only ritual analyses in hybrid-encoder fields; if penalties exceed baseline field noise, victimhood is structurally real.',
    'If no real cost is borne, the constraint should reclassify toward rope; if costs are real (funding denials, peer-review penalties), tangled_rope is sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(analyst_victimhood_validity, empirical, 'Whether analyst frustration constitutes structural victimhood').

omega_variable(
    hybrid_encoding_kernel_status,
    'Is the dual-register hybrid encoding a universal feature of catastrophe-survival ritual, or a contingent post-hoc reading imposed by modern theorists?',
    'Cross-cultural comparative analysis of pre-literate catastrophe rituals for independent emergence of dual-register encoding; if absent, the reading is analytically constructed.',
    'If constructed, the constraint''s Îµ should rise (extraction from academic discourse itself) and its coordination function may be weaker than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_encoding_kernel_status, conceptual, 'Contingency of the hybrid reading relative to the kernel').

omega_variable(
    practice_drift_modernity,
    'Has the practical survival register atrophied in literate or archival societies, leaving a symbol-only practice that still claims hybrid status?',
    'Ethnographic measurement of the ratio of actionable survival content to symbolic content in rituals across literacy gradients.',
    'If the practical register has atrophied, the constraint is sliding toward symbol-only or piton status, raising theater_ratio and lowering genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_drift_modernity, empirical, 'Modernity-driven drift in the practical register').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 50, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_survival__hybrid_encoding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, competence_transmission_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_survival kernel decomposes into three structurally distinct constraints (three readings). Each reading carries a different Îµ, beneficiary set, and coordination/extraction balance. This story instantiates the hybrid reading; the other two are separate JSON files in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
