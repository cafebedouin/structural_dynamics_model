% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__study_as_performance, []).

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
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Study of Sacrifice Law as Performance of Commandment
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This constraint represents the theological interpretation within Halakhic
 *   Judaism that the intellectual study of the laws pertaining to Temple
 *   sacrifices is itself considered a fulfillment of the divine commandment
 *   to offer sacrifices. This reading emerged after the destruction of the
 *   Second Temple, providing a means for adherents to engage with a central
 *   religious obligation when physical performance became impossible. It
 *   frames intellectual engagement as a form of worship and divine service.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.05).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.1).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Performance of Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious_studies/halakhic_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'b22d382c-f4c8-4fb3-abe2-7cb71238fd36').
narrative_ontology:cs_kernel_codification('b22d382c-f4c8-4fb3-abe2-7cb71238fd36', fixed_text).
narrative_ontology:cs_authority_grounding('b22d382c-f4c8-4fb3-abe2-7cb71238fd36', lineage).
narrative_ontology:cs_interpretation_layer_present('b22d382c-f4c8-4fb3-abe2-7cb71238fd36').
narrative_ontology:cs_reading_relation('b22d382c-f4c8-4fb3-abe2-7cb71238fd36', sacrifice_commandment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('b22d382c-f4c8-4fb3-abe2-7cb71238fd36', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('b22d382c-f4c8-4fb3-abe2-7cb71238fd36', foundational, intellectual_engagement_as_divine_service).
narrative_ontology:cs_axiom_status(intellectual_engagement_as_divine_service, holdable).
narrative_ontology:cs_axiom_grounding('b22d382c-f4c8-4fb3-abe2-7cb71238fd36', intellectual_engagement_as_divine_service, deontological).
narrative_ontology:cs_reference_frame('b22d382c-f4c8-4fb3-abe2-7cb71238fd36', post_temple_rabbinic_halakha).
narrative_ontology:cs_drift_state('b22d382c-f4c8-4fb3-abe2-7cb71238fd36', contemporary_secular_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('b22d382c-f4c8-4fb3-abe2-7cb71238fd36', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__study_as_performance, lay_adherents).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, intellectual_devotion_as_worship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who engage in the intellectual study of sacrifice law, believing this act itself fulfills a divine commandment and provides spiritual merit. Their identity is deeply intertwined with this devotional practice.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, beneficiary,
    moderate, biographical, identity_locked, global).

% The rabbinic and scholarly bodies that interpret, transmit, and uphold the tradition, affirming the validity and spiritual efficacy of studying sacrifice law as a form of worship, especially in the absence of a Temple.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Members of the community who adhere to the interpretive tradition. While they may not engage in deep scholarly study themselves, they accept its validity and may feel a spiritual burden if they cannot participate, yet benefit from the spiritual framework it provides.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, lay_adherents, payer,
    moderate, biographical, constrained, global).

% Those who maintain that the sacrifice commandment requires physical execution in a rebuilt Temple, viewing study as a preparation for future performance rather than a present fulfillment. Their perspective is marginalized by this reading.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, traditional_ritualists, excluded,
    moderate, biographical, identity_locked, global).

% Scholars who analyze the theological, historical, and philosophical implications of this interpretation, examining its development and impact on religious practice and thought.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, analytical_theologians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the devotional practice of adherents by providing a legitimate and spiritually meaningful path to fulfill the divine commandment of sacrifice in the absence of a physical Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit, divine favor, and a sense of communal continuity to scholar-worshippers and, indirectly, to the wider community through their intellectual engagement with sacred texts.
% ABSENT_VOICES: Traditional ritualists who insist on physical performance of sacrifices, and those who believe the commandment is entirely suspended without a Temple, are excluded from the interpretive framework that validates study as performance.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, a central pillar of post-Temple devotional practice would collapse, leaving a significant spiritual void for many adherents and potentially leading to a crisis of faith regarding the ongoing relevance of the sacrifice commandment.
% FOUNDING_PROBLEM: The core problem was how to fulfill the divine commandment of sacrifice after the destruction of the Second Temple, when physical performance became impossible, threatening the continuity of a central religious obligation.
% FOUNDING_PROBLEM_CORROBORATION: Centuries of rabbinic literature, halakhic responsa, and the continuous practice of Jewish scholarship attest to the enduring nature of this problem and the widespread acceptance of study as its resolution. This corroboration comes from within the tradition but is widely documented and historically verifiable.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).
:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a 'Rope' because it solves a genuine collective-action problem (how to fulfill a commandment without a Temple) with minimal coercive overhead. Extraction is negligible (0.05) as study is intrinsically valued and spiritually beneficial, not a burden. Suppression is low (0.1) because it is a devotional path chosen by adherents, not externally enforced. Theater ratio is low (0.05) as the act of study is considered genuinely functional worship within this framework. Accessibility collapse is moderate (0.5) because while physical sacrifice is impossible, other forms of devotion exist, but this specific path is a primary one for this commandment. Resistance is low (0.1) as it is a widely accepted and spiritually fulfilling practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of scholar-worshippers and halakhic authorities, this constraint is a pure coordination mechanism, enabling spiritual fulfillment and communal continuity. From the perspective of traditional ritualists (an excluded voice), it might be seen as a 'Piton' or 'Snare'—a theatrical substitute that extracts spiritual energy without genuine fulfillment, or even as a 'Tangled Rope' that coordinates a community around a flawed premise while extracting adherence to a less-than-ideal solution.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholar-worshippers are the primary beneficiaries (d=0.0) as they directly fulfill a divine obligation and gain spiritual merit. Halakhic authorities are agenda-setters who benefit from the continuity and legitimacy of the tradition. Lay adherents are indirect payers (d=0.5) as they adhere to the framework and may feel a burden if they cannot engage in deep study, but also benefit from the spiritual structure. There are no direct victims as the constraint is framed as a path to fulfillment, not extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    validity_of_study_as_performance,
    'Is the ''study_as_performance'' reading a valid fulfillment of the sacrifice commandment, or a necessary but ultimately incomplete substitution for physical performance?',
    'Theological consensus across different interpretive traditions, or a future restoration of the Temple allowing physical sacrifice to resume.',
    'If deemed an incomplete substitution, the constraint''s classification might shift towards a ''Scaffold'' (temporary support) or even a ''Piton'' (atrophied function) for those who adhere to it, as its foundational premise of full fulfillment would be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validity_of_study_as_performance, conceptual, 'Ambiguity of study as full performance vs. temporary substitution.').

omega_variable(
    impact_of_performance_only_reading,
    'How would the dominance of the ''performance_only'' sibling reading structurally alter the ''study_as_performance'' constraint?',
    'Analysis of shifts in communal adherence, rabbinic rulings, and resource allocation towards physical preparation over intellectual study.',
    'If ''performance_only'' were to become the dominant interpretation, ''study_as_performance'' would likely be reclassified as a ''Piton'' or ''Snare'' for its adherents, as its core claim of present fulfillment would be directly contradicted and its spiritual benefits undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impact_of_performance_only_reading, conceptual, 'Structural impact of the ''performance_only'' sibling reading.').

omega_variable(
    influence_of_archive_maintenance_reading,
    'How does the ''archive_maintenance'' sibling reading influence the ''study_as_performance'' reading''s emphasis and practice?',
    'Examination of curricula in religious academies, scholarly publications, and public discourse regarding the primary purpose of studying sacrifice law.',
    'If ''archive_maintenance'' gains prominence, the devotional aspect of ''study_as_performance'' might be diminished, shifting its classification towards a ''Rope'' focused purely on knowledge preservation rather than direct worship, potentially reducing its perceived spiritual efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(influence_of_archive_maintenance_reading, conceptual, 'Influence of the ''archive_maintenance'' sibling reading on devotional emphasis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__study_as_performance, theater_ratio, 20, 0.05).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__study_as_performance, theater_ratio, 40, 0.05).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__study_as_performance, theater_ratio, 60, 0.05).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_commandment__study_as_performance, theater_ratio, 80, 0.05).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__study_as_performance, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__study_as_performance, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__study_as_performance, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__study_as_performance, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(sacr_be_t80, sacrifice_commandment__study_as_performance, base_extractiveness, 80, 0.05).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__study_as_performance, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__study_as_performance, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sacr_su_t20, sacrifice_commandment__study_as_performance, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(sacr_su_t40, sacrifice_commandment__study_as_performance, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(sacr_su_t60, sacrifice_commandment__study_as_performance, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(sacr_su_t80, sacrifice_commandment__study_as_performance, suppression_requirement, 80, 0.1).
narrative_ontology:measurement(sacr_su_t100, sacrifice_commandment__study_as_performance, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'sacrifice_commandment' kernel, each with different structural properties and implications for adherents. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
