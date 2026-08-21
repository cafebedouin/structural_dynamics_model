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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_commandment__study_as_performance
 *   human_readable: Sacrifice Commandment: Study as Performance
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This constraint represents the 'study as performance' reading of the
 *   sacrifice commandment within Halakhic (Jewish legal) theory. It posits
 *   that intellectual engagement with the laws of sacrifice is itself a
 *   fulfillment of the divine obligation, particularly relevant in the
 *   absence of a functioning Temple. This reading emphasizes the intrinsic
 *   value of Torah study as a form of worship. The constraint is classified
 *   as a Mountain due to its perceived naturalness within this theological
 *   framework, with zero extraction and suppression, as study is seen as a
 *   pure, uncoerced act of devotion.
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
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(sacrifice_commandment__study_as_performance, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__study_as_performance, mountain).
narrative_ontology:human_readable(sacrifice_commandment__study_as_performance, "Sacrifice Commandment: Study as Performance").
narrative_ontology:topic_domain(sacrifice_commandment__study_as_performance, "religious_studies/halakhic_theory").

domain_priors:emerges_naturally(sacrifice_commandment__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'f54eec3e-0055-4849-b6ce-ccc19c426b46').
narrative_ontology:cs_kernel_codification('f54eec3e-0055-4849-b6ce-ccc19c426b46', fixed_text).
narrative_ontology:cs_authority_grounding('f54eec3e-0055-4849-b6ce-ccc19c426b46', lineage).
narrative_ontology:cs_interpretation_layer_present('f54eec3e-0055-4849-b6ce-ccc19c426b46').
narrative_ontology:cs_reading_relation('f54eec3e-0055-4849-b6ce-ccc19c426b46', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('f54eec3e-0055-4849-b6ce-ccc19c426b46', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('f54eec3e-0055-4849-b6ce-ccc19c426b46', foundational, torah_study_is_divine_service).
narrative_ontology:cs_axiom_status(torah_study_is_divine_service, holdable).
narrative_ontology:cs_axiom_grounding('f54eec3e-0055-4849-b6ce-ccc19c426b46', torah_study_is_divine_service, deontological).
narrative_ontology:cs_axiom('f54eec3e-0055-4849-b6ce-ccc19c426b46', foundational, intellectual_engagement_fulfills_mitzvah).
narrative_ontology:cs_axiom_status(intellectual_engagement_fulfills_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('f54eec3e-0055-4849-b6ce-ccc19c426b46', intellectual_engagement_fulfills_mitzvah, deontological).
narrative_ontology:cs_reference_frame('f54eec3e-0055-4849-b6ce-ccc19c426b46', post_temple_rabbinic_tradition).
narrative_ontology:cs_drift_state('f54eec3e-0055-4849-b6ce-ccc19c426b46', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f54eec3e-0055-4849-b6ce-ccc19c426b46', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__study_as_performance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, scholar_worshippers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__study_as_performance, lay_adherents).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, torah_lischma_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__study_as_performance, intellectual_worship_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who engage in the intellectual study of sacrifice laws, believing this act itself fulfills a divine commandment and constitutes a form of worship. They derive spiritual and intellectual benefit from this engagement.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, scholar_worshippers, beneficiary,
    moderate, biographical, mobile, global).

% Religious scholars and leaders who interpret and transmit Jewish law. They affirm and teach the doctrine that study of sacrifice laws is equivalent to their performance, guiding the community in this form of worship.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Members of the religious community who may not be scholars themselves but are guided by the teaching that study fulfills the commandment. They benefit from the spiritual access this interpretation provides, even without a functioning Temple.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, lay_adherents, beneficiary,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally accessible and intellectually rigorous means for adherents to fulfill a central divine commandment (sacrifice) in the absence of a physical Temple, coordinating spiritual practice across time and space.
% TRANSFER_FUNCTION: Transfers spiritual merit and divine favor to the scholar-worshipper through intellectual engagement, transforming abstract legal knowledge into an act of worship.
% ABSENT_VOICES: Those who insist on literal physical performance of sacrifices would object, arguing that study is a substitute, not a fulfillment. They are present in other interpretive communities but excluded from this reading's core premise.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, a significant pathway for divine service and spiritual engagement for many adherents would disappear, leading to a crisis of religious practice and meaning in the absence of a Temple. The religious landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the physical performance of sacrifices impossible, creating a void in central divine worship and a challenge to the continuity of religious obligation.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts and ongoing theological discourse from diverse Jewish traditions corroborate the problem of Temple destruction and the need for alternative forms of divine service. This is widely attested across the religious community, not just by those who benefit from this specific interpretation.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_commandment__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__study_as_performance, 0.0, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The extractiveness is zero because study is considered an intrinsically valuable act of worship, not a burden or a means of extraction. Suppression is zero as intellectual engagement is voluntary and accessible. Theater ratio is zero because the act of study is the direct fulfillment, not a performance masking another function. Accessibility collapse is high (0.9) because, within this framework, the path to fulfilling the commandment through study is clear and universally available, making other (physical) alternatives irrelevant or impossible. Resistance is zero as this interpretation is embraced by its adherents as a spiritual solution.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap within this reading, as all participants view study as a beneficial and fulfilling act. However, this reading itself stands in contrast to other interpretations of the sacrifice commandment, which would generate significant perspectival gaps.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholar-worshippers and lay adherents are beneficiaries, as they gain spiritual merit and fulfill a divine obligation without cost. Halakhic authorities are agenda-setters, guiding this interpretation. There are no victims, as no party is coerced or extracted from; the act is one of voluntary devotion.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in this reading, as the 'mandate' (divine commandment) is considered eternally live, and the 'function' (fulfillment through study) remains perpetually relevant in the absence of a Temple. The classification as a Mountain reflects this perceived timeless and unchangeable nature within its theological context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_naturalness_ambiguity,
    'Is the ''study as performance'' interpretation a genuine theological natural law, or a constructed theological response to historical circumstances?',
    'Comparative theological analysis across diverse religious traditions regarding the nature of divine commandments and their fulfillment in changed circumstances. Examination of historical shifts in rabbinic discourse.',
    'If constructed, the constraint''s ''emerges_naturally'' claim would be reclassified, potentially shifting its type from Mountain to Rope (if purely coordinative) or even Snare (if used to maintain institutional power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_naturalness_ambiguity, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of the theological interpretation.').

omega_variable(
    fulfillment_equivalence_ambiguity,
    'To what extent is intellectual study truly equivalent to physical performance in fulfilling the divine commandment, or is it a lesser, albeit necessary, substitute?',
    'Further theological and philosophical inquiry into the nature of ritual, intention (kavvanah), and action in religious law. Examination of the hierarchy of mitzvot (commandments).',
    'If study is deemed a lesser substitute, the ''zero extractiveness'' claim might be challenged, as adherents might experience a subtle ''extraction'' of full spiritual potential, or a ''theater_ratio'' might emerge if the performance is seen as merely symbolic rather than fully equivalent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fulfillment_equivalence_ambiguity, conceptual, 'Ambiguity regarding the full equivalence of study to physical ritual performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_commandment__study_as_performance, theater_ratio, 25, 0.0).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_commandment__study_as_performance, theater_ratio, 50, 0.0).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_commandment__study_as_performance, theater_ratio, 75, 0.0).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__study_as_performance, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__study_as_performance, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacr_be_t25, sacrifice_commandment__study_as_performance, base_extractiveness, 25, 0.0).
narrative_ontology:measurement(sacr_be_t50, sacrifice_commandment__study_as_performance, base_extractiveness, 50, 0.0).
narrative_ontology:measurement(sacr_be_t75, sacrifice_commandment__study_as_performance, base_extractiveness, 75, 0.0).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__study_as_performance, base_extractiveness, 100, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__study_as_performance, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(sacr_su_t25, sacrifice_commandment__study_as_performance, suppression_requirement, 25, 0.0).
narrative_ontology:measurement(sacr_su_t50, sacrifice_commandment__study_as_performance, suppression_requirement, 50, 0.0).
narrative_ontology:measurement(sacr_su_t75, sacrifice_commandment__study_as_performance, suppression_requirement, 75, 0.0).
narrative_ontology:measurement(sacr_su_t100, sacrifice_commandment__study_as_performance, suppression_requirement, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__study_as_performance, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
