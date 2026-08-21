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
 *   This constraint represents a specific reading within Halakhic (Jewish
 *   legal) theory, where the intellectual study of the laws pertaining to
 *   Temple sacrifices is considered a direct fulfillment of the divine
 *   commandment to offer sacrifices. In the absence of the Temple, this
 *   reading posits that scholarly engagement is not a substitute but an
 *   equivalent act of worship, fulfilling a divine obligation through
 *   intellectual means. This story instantiates the 'study_as_performance'
 *   reading of the 'sacrifice_commandment' kernel.
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
narrative_ontology:cs_story_uid(sacrifice_commandment__study_as_performance, 'e49fac4f-1511-4447-b69d-08870a1a398a').
narrative_ontology:cs_kernel_codification('e49fac4f-1511-4447-b69d-08870a1a398a', fixed_text).
narrative_ontology:cs_authority_grounding('e49fac4f-1511-4447-b69d-08870a1a398a', lineage).
narrative_ontology:cs_interpretation_layer_present('e49fac4f-1511-4447-b69d-08870a1a398a').
narrative_ontology:cs_reading_relation('e49fac4f-1511-4447-b69d-08870a1a398a', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('e49fac4f-1511-4447-b69d-08870a1a398a', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('e49fac4f-1511-4447-b69d-08870a1a398a', foundational, intellectual_engagement_as_divine_service).
narrative_ontology:cs_axiom_status(intellectual_engagement_as_divine_service, holdable).
narrative_ontology:cs_axiom_grounding('e49fac4f-1511-4447-b69d-08870a1a398a', intellectual_engagement_as_divine_service, deontological).
narrative_ontology:cs_axiom('e49fac4f-1511-4447-b69d-08870a1a398a', foundational, study_as_equivalent_to_action).
narrative_ontology:cs_axiom_status(study_as_equivalent_to_action, holdable).
narrative_ontology:cs_axiom_grounding('e49fac4f-1511-4447-b69d-08870a1a398a', study_as_equivalent_to_action, theological).
narrative_ontology:cs_reference_frame('e49fac4f-1511-4447-b69d-08870a1a398a', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('e49fac4f-1511-4447-b69d-08870a1a398a', contemporary_religious_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e49fac4f-1511-4447-b69d-08870a1a398a', '').
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

% Religious scholars and leaders who interpret and transmit Jewish law. They affirm and teach the validity of study as a fulfillment of the commandment, shaping the discourse and practice for their communities.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Members of the religious community who may not engage in deep scholarly study themselves but benefit from the spiritual and communal framework provided by this interpretation, finding meaning and continuity in the absence of physical sacrifice.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__study_as_performance, lay_adherents, beneficiary,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a pathway for individuals to fulfill a central divine commandment (sacrifice) through intellectual and spiritual engagement, maintaining continuity of religious practice in the absence of the Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit and divine favor to the scholar-worshipper through the act of study, transforming intellectual effort into religious observance.
% ABSENT_VOICES: Those who insist on the literal, physical performance of sacrifice as the only fulfillment of the commandment, viewing study as a substitute but not an equivalent. They are present in the broader halakhic discourse but their view is not central to this reading.
% DISAPPEARANCE_RATIONALE: If the understanding that study fulfills the sacrifice commandment vanished, a significant spiritual and intellectual practice would cease to be recognized as divine service, leaving a void in religious observance and potentially leading to a sense of unfulfilled obligation for many adherents.
% FOUNDING_PROBLEM: The destruction of the Temple and the cessation of physical sacrifices left a central divine commandment unfulfillable, creating a crisis of religious practice and meaning.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing absence of the Temple and the continued centrality of the sacrifice commandment in religious texts are attested by all religious authorities and historical records, corroborating the live status of the founding problem.
narrative_ontology:disappearance_verdict(sacrifice_commandment__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Mountain because it is understood as an intrinsic spiritual truth within this theological framework, offering a path to divine connection without extraction. Extractiveness, suppression, and theater ratio are all zero, reflecting the belief that this is a pure, uncoerced act of worship. Accessibility collapse is high (0.9) because, within this framework, the truth of study as performance is considered self-evident and universally applicable to those who accept the premise, leaving no 'alternative' way to fulfill the commandment through study other than to simply do it. Resistance is zero as it is an internal theological principle.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap within this reading, as all adherents who accept its premise experience it as a non-extractive, intrinsically valuable form of worship. The divergence arises when comparing this reading to other interpretations of the sacrifice commandment.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholar-worshippers are direct beneficiaries, as they fulfill a divine obligation and gain spiritual merit. Halakhic authorities are agenda-setters, guiding the interpretation and practice. Lay adherents benefit from the spiritual framework. There are no victims, as the act is voluntary and intrinsically rewarding.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy within its own framework, as the 'mandate' (divine commandment) is considered eternal and the 'function' (fulfillment through study) is seen as a direct and ongoing response to the historical reality of the Temple's destruction. The classification as a Mountain reflects its perceived naturalness and immutability within this theological system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, independent ''Mountain'' of theological truth, or a constructed ''Rope'' of coordination that provides a functional substitute for an unfulfillable commandment?',
    'Analysis of the broader theological discourse and the historical development of this interpretation: if it emerged primarily as a pragmatic solution to a crisis, it leans towards ''Rope''; if it is rooted in deeper, pre-existing theological principles of intellectual worship, it leans towards ''Mountain''.',
    'If reclassified as a ''Rope'', it would imply a coordination function with potential for subtle extraction or dependence, altering its perceived purity and the nature of its ''beneficiaries''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between intrinsic theological truth and functional coordination.').

omega_variable(
    sibling_reading_impact_performance_only,
    'How would the ''performance_only'' reading (sacrifice commandment requires physical execution; without Temple, commandment is suspended not fulfilled) structurally alter this ''study_as_performance'' reading if it gained dominance?',
    'Observing shifts in communal practice and theological emphasis: if ''performance_only'' became dominant, ''study_as_performance'' would likely be reclassified as a ''Scaffold'' (temporary substitute) or ''Piton'' (inertial practice) rather than a direct fulfillment.',
    'The ''study_as_performance'' reading would lose its ''Mountain'' status and be re-evaluated as a less direct or even invalid form of fulfillment, leading to a crisis of meaning for its adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_performance_only, conceptual, 'Impact of the ''performance_only'' sibling reading on the ''study_as_performance'' reading.').

omega_variable(
    sibling_reading_impact_archive_maintenance,
    'How would the ''archive_maintenance'' reading (study preserves technical knowledge for future Temple restoration; messianic preparation not present worship) structurally alter this ''study_as_performance'' reading if it gained dominance?',
    'Observing shifts in the stated purpose and communal understanding of sacrifice study: if ''archive_maintenance'' became dominant, ''study_as_performance'' would likely be reclassified as a ''Rope'' (coordination for future action) rather than a direct fulfillment.',
    'The ''study_as_performance'' reading would lose its ''Mountain'' status and be re-evaluated as a preparatory act rather than a present fulfillment, diminishing its spiritual immediacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_archive_maintenance, conceptual, 'Impact of the ''archive_maintenance'' sibling reading on the ''study_as_performance'' reading.').


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
