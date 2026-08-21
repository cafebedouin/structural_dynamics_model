% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deletionist_reading, []).

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
 *   constraint_id: notability_guidelines__deletionist_reading
 *   human_readable: Wikipedia Notability Guidelines (Deletionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This constraint story represents the 'deletionist reading' of Wikipedia's
 *   Notability Guidelines (WP:N). From this perspective, WP:N functions as a
 *   crucial epistemic quality filter, essential for preventing the
 *   degradation of Wikipedia as a reliable digital commons. It ensures that
 *   content meets standards of verifiability and significance, thereby
 *   protecting the integrity and utility of the encyclopedia for its
 *   readership.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.15).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.2).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Guidelines (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, '645bb39a-e2c6-4e1b-a943-725d71588a30').
narrative_ontology:cs_kernel_codification('645bb39a-e2c6-4e1b-a943-725d71588a30', fixed_text).
narrative_ontology:cs_authority_grounding('645bb39a-e2c6-4e1b-a943-725d71588a30', practice).
narrative_ontology:cs_interpretation_layer_present('645bb39a-e2c6-4e1b-a943-725d71588a30').
narrative_ontology:cs_reading_relation('645bb39a-e2c6-4e1b-a943-725d71588a30', notability_guidelines__inclusionist_reading, forecloses).
narrative_ontology:cs_reading_relation('645bb39a-e2c6-4e1b-a943-725d71588a30', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('645bb39a-e2c6-4e1b-a943-725d71588a30', foundational, notability_is_objective_quality_criterion).
narrative_ontology:cs_axiom_status(notability_is_objective_quality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('645bb39a-e2c6-4e1b-a943-725d71588a30', notability_is_objective_quality_criterion, empirically_contingent).
narrative_ontology:cs_axiom('645bb39a-e2c6-4e1b-a943-725d71588a30', foundational, unfiltered_commons_degrades).
narrative_ontology:cs_axiom_status(unfiltered_commons_degrades, holdable).
narrative_ontology:cs_axiom_grounding('645bb39a-e2c6-4e1b-a943-725d71588a30', unfiltered_commons_degrades, empirically_contingent).
narrative_ontology:cs_reference_frame('645bb39a-e2c6-4e1b-a943-725d71588a30', encyclopedic_quality_preservation).
narrative_ontology:cs_drift_state('645bb39a-e2c6-4e1b-a943-725d71588a30', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('645bb39a-e2c6-4e1b-a943-725d71588a30', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_editors_deletionist).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, wikipedia_editors_inclusionist).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a high-quality, reliable encyclopedia free from spam, vanity, and trivial content. Their continued engagement is contingent on the perceived quality and trustworthiness of Wikipedia.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_readership, beneficiary,
    moderate, biographical, mobile, global).

% Actively enforce notability guidelines, viewing them as essential for maintaining Wikipedia's encyclopedic mission and preventing content degradation. They invest significant time and effort in content curation and deletion processes.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_editors_deletionist, agenda_setter,
    institutional, generational, identity_locked, global).

% While participating in the same system, they often bear the cost of defending articles or topics they believe are notable but face deletion challenges. From the deletionist perspective, their efforts are sometimes misdirected, but they are not 'victims' of extraction, merely participants in a necessary quality control process.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_editors_inclusionist, payer,
    organized, biographical, constrained, global).

% Attempt to use Wikipedia for self-promotion or non-encyclopedic purposes. They are systematically excluded by the notability guidelines, which the deletionist reading considers a just and necessary function to protect the commons.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, spam_vanity_contributors, excluded,
    powerless, immediate, trapped, global).

% Seek to include topics or perspectives that may struggle to meet traditional notability criteria, often due to systemic biases in source availability. From the deletionist perspective, their content is excluded if it lacks sufficient verifiable sources, not due to malice, but due to the epistemic filter.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, marginalized_knowledge_advocates, excluded,
    moderate, generational, constrained, global).

% Provides the technical and legal infrastructure for Wikipedia, but largely defers to the community's self-governance, including the application of notability guidelines. They observe the debates and outcomes without directly dictating content policy.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_foundation, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deletionist_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deletionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate editorial effort towards maintaining a high-quality, verifiable, and encyclopedic knowledge base, preventing the degradation of the digital commons by non-notable or unreliable content.
% TRANSFER_FUNCTION: Transfers editorial authority and content control to established editors and community consensus, preventing the consumption of finite editorial resources by content that does not meet encyclopedic standards.
% ABSENT_VOICES: Spam and vanity contributors, as well as advocates for topics that consistently fail to meet the established notability criteria, are effectively excluded from contributing their desired content. They would argue for a more open or less stringent inclusion policy.
% DISAPPEARANCE_RATIONALE: If notability guidelines and their enforcement vanished, Wikipedia would rapidly become an unmanageable repository of self-promotion, trivial information, and unverified claims, losing its utility as a reliable encyclopedia and driving away its readership and dedicated editors.
% FOUNDING_PROBLEM: Preventing Wikipedia from becoming a chaotic, unreliable, and unmanageable repository of non-encyclopedic content, ensuring its long-term viability as a trusted knowledge resource.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing challenges of content moderation on open platforms, the historical experience of other wikis that lacked strict notability rules (leading to degradation), and academic studies on information quality in user-generated content platforms, all corroborate the persistent need for such filters.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deletionist_reading_tests).
:- end_tests(notability_guidelines__deletionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because, from the deletionist perspective, it provides a genuine coordination function (quality control) with minimal extraction. Base extractiveness (0.15) is low, representing the necessary overhead of maintaining quality standards. Suppression (0.20) is also low, reflecting that exclusion is seen as a consequence of failing to meet objective criteria, not coercion. Theater ratio (0.10) is low, indicating that the guidelines are primarily functional. Accessibility collapse (0.80) is high because once the rules are understood, the 'alternative' of including non-notable content is effectively closed. Resistance (0.70) is high due to ongoing debates and challenges to notability decisions, which are viewed as part of the necessary, albeit sometimes contentious, process of maintaining quality.
 *
 * PERSPECTIVAL GAP:
 *   This story explicitly adopts the deletionist perspective. Other readings (inclusionist, deliberative) would assign different metric values and classifications, particularly regarding extraction and victimhood. The engine's classification will reflect this specific reading's structural data, not an average or synthesis of all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The Wikipedia readership and deletionist editors are the primary beneficiaries, gaining from a high-quality, reliable encyclopedia. There are no 'victims' from this reading, as those whose content is excluded (e.g., spam/vanity contributors, or advocates for non-notable topics) are seen as justly filtered to protect the commons. Inclusionist editors, while sometimes bearing the cost of defending articles, are considered participants in the overall quality control mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'Is this constraint a genuine epistemic filter (deletionist reading), or a gatekeeping apparatus (inclusionist reading), or a perpetual negotiation (deliberative reading)?',
    'Analysis of power dynamics in AfD, empirical studies of content inclusion/exclusion patterns, and the impact of guideline changes on marginalized communities.',
    'If reclassified as an inclusionist reading, extractiveness and suppression would be higher, and there would be identifiable victims. If reclassified as a deliberative reading, the focus would shift to the process itself, potentially altering the perception of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Ambiguity of notability guidelines as filter vs. gatekeeping vs. process.').

omega_variable(
    justly_excluded_vs_victim,
    'Are those whose content is excluded by WP:N ''justly excluded'' (deletionist view) or ''victims'' of a structural gatekeeping mechanism (inclusionist view)?',
    'Examination of the systemic biases in source availability and the impact of notability criteria on the representation of diverse knowledge systems. If exclusion correlates with systemic disadvantage rather than objective quality, the ''justly excluded'' claim weakens.',
    'If reclassified as victims, the constraint''s effective extraction would be higher, and the classification might shift towards a Tangled Rope or Snare, depending on the degree of coercion and benefit asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justly_excluded_vs_victim, empirical, 'Whether exclusion by notability guidelines constitutes victimhood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__deletionist_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__deletionist_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__deletionist_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(nota_tr_t25, notability_guidelines__deletionist_reading, theater_ratio, 25, 0.1).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__deletionist_reading, base_extractiveness, 5, 0.11).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__deletionist_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__deletionist_reading, base_extractiveness, 15, 0.13).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(nota_be_t25, notability_guidelines__deletionist_reading, base_extractiveness, 25, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__deletionist_reading, suppression_requirement, 5, 0.16).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__deletionist_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__deletionist_reading, suppression_requirement, 15, 0.18).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deletionist_reading, suppression_requirement, 20, 0.19).
narrative_ontology:measurement(nota_su_t25, notability_guidelines__deletionist_reading, suppression_requirement, 25, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
