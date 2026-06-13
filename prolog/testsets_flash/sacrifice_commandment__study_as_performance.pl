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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   This constraint represents a specific reading within Halakhic (Jewish
 *   legal) theory, where the intellectual engagement with the laws of Temple
 *   sacrifices is considered a direct fulfillment of the divine commandment,
 *   rather than merely a preparation for a future physical performance. This
 *   reading emerged after the destruction of the Second Temple, providing a
 *   continuous mode of worship and observance. It is framed as a Mountain
 *   because, within this interpretive tradition, the spiritual efficacy of
 *   study is considered an intrinsic, unchangeable truth, offering profound
 *   spiritual benefit without extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:suppression_score(sacrifice_commandment__study_as_performance, 0.0).
domain_priors:theater_ratio(sacrifice_commandment__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, mountain).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Study of Sacrifice Law as Performance of Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious_studies/halakhic_theory").

domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'e225dfac-86a3-4b39-bfcd-b93b735a5225').
narrative_ontology:cs_kernel_codification('e225dfac-86a3-4b39-bfcd-b93b735a5225', fixed_text).
narrative_ontology:cs_authority_grounding('e225dfac-86a3-4b39-bfcd-b93b735a5225', lineage).
narrative_ontology:cs_interpretation_layer_present('e225dfac-86a3-4b39-bfcd-b93b735a5225').
narrative_ontology:cs_reading_relation('e225dfac-86a3-4b39-bfcd-b93b735a5225', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('e225dfac-86a3-4b39-bfcd-b93b735a5225', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('e225dfac-86a3-4b39-bfcd-b93b735a5225', foundational, intellectual_engagement_is_divine_service).
narrative_ontology:cs_axiom_status(intellectual_engagement_is_divine_service, holdable).
narrative_ontology:cs_axiom_grounding('e225dfac-86a3-4b39-bfcd-b93b735a5225', intellectual_engagement_is_divine_service, theological).
narrative_ontology:cs_axiom('e225dfac-86a3-4b39-bfcd-b93b735a5225', foundational, divine_commandment_is_always_fulfillable).
narrative_ontology:cs_axiom_status(divine_commandment_is_always_fulfillable, holdable).
narrative_ontology:cs_axiom_grounding('e225dfac-86a3-4b39-bfcd-b93b735a5225', divine_commandment_is_always_fulfillable, deontological).
narrative_ontology:cs_reference_frame('e225dfac-86a3-4b39-bfcd-b93b735a5225', post_temple_rabbinic_consensus).
narrative_ontology:cs_drift_state('e225dfac-86a3-4b39-bfcd-b93b735a5225', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e225dfac-86a3-4b39-bfcd-b93b735a5225', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshipper).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% For the scholar-worshipper, the act of studying the intricate laws of sacrifice is itself a form of divine worship and fulfillment of the commandment, providing spiritual satisfaction and a sense of purpose. Their identity is deeply intertwined with this intellectual and spiritual engagement.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshipper, beneficiary,
    moderate, biographical, identity_locked, local).

% These authorities interpret and transmit the tradition, affirming the validity and spiritual efficacy of study as a substitute for physical sacrifice. They guide the community in understanding this mode of observance.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Members of the religious community benefit from the spiritual continuity and communal identity fostered by this interpretive tradition, even if they are not actively engaged in scholarly study themselves. They receive guidance and reassurance from the halakhic authorities.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, community_members, beneficiary,
    powerless, biographical, identity_locked, local).

% Interpretations that insist on the physical performance of sacrifice as the only valid fulfillment of the commandment are marginalized by this reading. They would argue that study is preparation, not performance, but their voice is not central to this tradition.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, literalist_interpretations, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a continuous, accessible means for individuals to fulfill a central divine commandment even in the absence of a physical Temple, maintaining spiritual connection and communal identity.
% TRANSFER_FUNCTION: Transforms intellectual and spiritual effort into divine fulfillment and merit, transferring a sense of religious obligation from physical action to mental engagement.
% ABSENT_VOICES: Literalist interpretations that insist on physical performance as the sole fulfillment are excluded; they would argue that the commandment is suspended, not fulfilled, by study. Their perspective is not integrated into the dominant halakhic discourse that upholds study as performance.
% DISAPPEARANCE_RATIONALE: If the understanding of study as performance vanished, a core mechanism for religious observance and spiritual continuity in the absence of the Temple would disappear. This would create a profound crisis of religious practice and identity for millions, necessitating a complete re-evaluation of divine obligation and worship.
% FOUNDING_PROBLEM: The destruction of the Temple rendered the physical performance of animal sacrifices impossible, creating a crisis of how to fulfill central divine commandments.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live as the Temple has not been rebuilt. The ongoing practice of study as performance, attested by centuries of rabbinic literature and continuous communal observance, corroborates the enduring nature of this foundational problem and the efficacy of this solution. This is corroborated by the lived experience of the community, not just the benefiting authorities.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_commandment__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_commandment__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is zero because study is seen as intrinsically valuable worship, not a burden or a means of extraction. There is no suppression, as participation is voluntary and spiritually rewarding. Theater ratio is zero because the activity is considered fully functional as worship. Accessibility collapse is high (0.95) because, within this framework, there are no viable alternatives for fulfilling the commandment in the absence of the Temple other than study. Resistance is low (0.05) because this reading is widely accepted and provides a vital spiritual outlet.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the scholar-worshipper, this is a pure Mountain, an unchangeable spiritual truth that provides immense benefit. From the perspective of a literalist, it might be seen as a 'snare' or 'tangled rope' if they feel coerced into an interpretation they don't accept, or a 'piton' if they see it as a theatrical substitute for a lost practice. However, this story strictly adheres to the 'study_as_performance' reading, where it is a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The scholar-worshipper is the primary beneficiary, receiving spiritual fulfillment and a sense of divine obligation met. Halakhic authorities are agenda-setters, guiding this interpretation. Community members are also beneficiaries, gaining spiritual continuity. Literalists are excluded, as their interpretation is not accommodated by this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by re-interpreting the mandate itself. The original mandate (physical sacrifice) is no longer performable, but the 'study as performance' reading ensures the underlying divine obligation remains 'live' and fulfillable, preventing the commandment from becoming a 'piton' (atrophied ritual) or a 'snare' (coercive, impossible demand). The mandate is transformed, not merely maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_interpretation,
    'Is the spiritual efficacy of ''study as performance'' a genuine natural law of divine interaction, or a constructed interpretive framework that benefits identifiable agents (scholar-worshippers, halakhic authorities)?',
    'Theological-philosophical analysis of the nature of divine commandments and human agency, or comparative study of how other traditions adapt core rituals to changed circumstances.',
    'If a constructed framework, the constraint might be reclassified from Mountain to Rope or even Tangled Rope, depending on the degree of implicit extraction or coordination involved in maintaining the interpretive consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_interpretation, conceptual, 'Ambiguity between intrinsic spiritual truth and interpretive construction.').

omega_variable(
    identity_lock_vs_genuine_benefit,
    'Is the ''identity_locked'' exit option for scholar-worshippers a reflection of genuine, uncoerced spiritual benefit, or a form of internalized suppression where identity fusion prevents critical evaluation of the constraint?',
    'Qualitative sociological study of individuals who have exited or re-evaluated this interpretive framework, examining their post-exit experiences and reasons for departure.',
    'If internalized suppression is a significant factor, the effective suppression for scholar-worshippers would be higher than the structural measure suggests, potentially shifting the constraint''s classification for that seat towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_genuine_benefit, empirical, 'Distinguishing genuine identity fusion from internalized suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_commandment__study_as_performance, theater_ratio, 50, 0.0).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__study_as_performance, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacr_be_t50, sacrifice_commandment__study_as_performance, base_extractiveness, 50, 0.0).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__study_as_performance, base_extractiveness, 100, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__study_as_performance, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(sacr_su_t50, sacrifice_commandment__study_as_performance, suppression_requirement, 50, 0.0).
narrative_ontology:measurement(sacr_su_t100, sacrifice_commandment__study_as_performance, suppression_requirement, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__performance_only).
narrative_ontology:affects_constraint(sacrifice_commandment__study_as_performance, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sacrifice_commandment' kernel. This 'study_as_performance' reading provides a continuous mode of observance, contrasting with 'performance_only' (which suspends the commandment) and 'archive_maintenance' (which sees study as preparation, not fulfillment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
