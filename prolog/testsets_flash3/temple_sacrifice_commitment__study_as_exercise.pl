% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Temple Sacrifice Commitment: Study as Exercise
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the reading within Halakhic tradition that
 *   views the intellectual study of Temple sacrifice laws as a direct and
 *   complete fulfillment of the divine command, particularly in the absence
 *   of the physical Temple. It is not a substitute, but an active exercise of
 *   commitment. This reading posits zero extractiveness, as the act of study
 *   is intrinsically valuable and beneficial to the studying community,
 *   maintaining covenant fidelity without imposing costs or suppressing
 *   alternatives for its adherents. It is claimed as a Mountain due to its
 *   perceived intrinsic and unchangeable nature within this interpretive
 *   framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Temple Sacrifice Commitment: Study as Exercise").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '78a7239a-3c95-42f6-b32e-68e86cdefcbd').
narrative_ontology:cs_kernel_codification('78a7239a-3c95-42f6-b32e-68e86cdefcbd', fixed_text).
narrative_ontology:cs_authority_grounding('78a7239a-3c95-42f6-b32e-68e86cdefcbd', lineage).
narrative_ontology:cs_interpretation_layer_present('78a7239a-3c95-42f6-b32e-68e86cdefcbd').
narrative_ontology:cs_reading_relation('78a7239a-3c95-42f6-b32e-68e86cdefcbd', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('78a7239a-3c95-42f6-b32e-68e86cdefcbd', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('78a7239a-3c95-42f6-b32e-68e86cdefcbd', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('78a7239a-3c95-42f6-b32e-68e86cdefcbd', foundational, intellectual_engagement_is_divine_service).
narrative_ontology:cs_axiom_status(intellectual_engagement_is_divine_service, holdable).
narrative_ontology:cs_axiom_grounding('78a7239a-3c95-42f6-b32e-68e86cdefcbd', intellectual_engagement_is_divine_service, deontological).
narrative_ontology:cs_axiom('78a7239a-3c95-42f6-b32e-68e86cdefcbd', foundational, divine_command_is_transcendent_of_material_conditions).
narrative_ontology:cs_axiom_status(divine_command_is_transcendent_of_material_conditions, holdable).
narrative_ontology:cs_axiom_grounding('78a7239a-3c95-42f6-b32e-68e86cdefcbd', divine_command_is_transcendent_of_material_conditions, theological).
narrative_ontology:cs_reference_frame('78a7239a-3c95-42f6-b32e-68e86cdefcbd', rabbinic_tradition_post_temple_destruction).
narrative_ontology:cs_drift_state('78a7239a-3c95-42f6-b32e-68e86cdefcbd', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('78a7239a-3c95-42f6-b32e-68e86cdefcbd', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, torah_study_as_divine_service).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, intellectual_engagement_as_spiritual_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community of scholars and adherents who engage in the intellectual study of sacrifice laws. They derive spiritual fulfillment and maintain covenant fidelity through this practice, seeing it as a direct fulfillment of divine command in the absence of the Temple.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, generational, identity_locked, global).

% The abstract concept of maintaining the covenant with the divine. This constraint ensures the continuity and active engagement with divine commands, even when their material performance is not possible.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity).

% The ultimate source of the obligation to engage with sacrifice laws. The study is understood as a direct response to this command, fulfilling its intent through intellectual and spiritual means.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, divine_command, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, divine_command).

% Those who believe that study is merely preparatory and that true fulfillment requires the physical rebuilding of the Temple and resumption of sacrifices. They are excluded from the 'study as full performance' framing, viewing it as a temporary substitute rather than an equivalent.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, messianic_restoration_advocates, excluded,
    moderate, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing spiritual and intellectual engagement of the community with divine commands concerning Temple sacrifice, ensuring continuity of religious practice and identity in the absence of the physical Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit and covenant fidelity from the act of study to the studying community, fulfilling the divine command through intellectual rather than material means.
% ABSENT_VOICES: Advocates for immediate material performance of sacrifices (e.g., certain messianic groups) would argue that study is insufficient and that the divine command requires physical action. They are absent from the interpretive framework that elevates study to full performance.
% DISAPPEARANCE_RATIONALE: If the understanding of 'study as exercise' vanished, a core mechanism for maintaining active covenant fidelity in the absence of the Temple would disappear. The religious community's engagement with a significant portion of its law would become purely archival, leading to a profound shift in spiritual practice and identity.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the material performance of divine commands regarding sacrifice impossible, creating a crisis of religious practice and continuity.
% FOUNDING_PROBLEM_CORROBORATION: The problem of how to fulfill divine commands related to sacrifice in the absence of the Temple remains a live theological and practical concern for the religious community, attested by centuries of rabbinic commentary and ongoing scholarly debate across various traditions.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.0, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness, suppression, and theater_ratio are all zero because, from this reading's perspective, the study is a pure, uncoerced act of devotion that benefits the participant directly without imposing external costs or requiring performative maintenance. Accessibility collapse is high (0.9) because, within this framework, the alternative of 'not studying' is seen as a collapse of spiritual engagement, while 'material performance' is currently impossible. Resistance is zero as the practice is embraced by its adherents. The constraint is considered a Mountain because its justification is rooted in a theological interpretation that posits the intrinsic and immutable value of study as divine service.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of the Temple sacrifice commitment (e.g., 'performance_only' or 'hybrid_preparatory') would experience this constraint differently, viewing 'study as exercise' as either an insufficient substitute or a temporary measure, rather than a full and equivalent performance. This reading, however, asserts its intrinsic and complete nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'studying_community' is the primary beneficiary, gaining spiritual merit and maintaining identity. 'Covenant_fidelity' is also a beneficiary, as the constraint ensures its continuity. The 'divine_command' is the agenda-setter, as it is the ultimate source of the obligation. There are no victims, as the practice is seen as purely beneficial. Directionality for beneficiaries is near 0.0, reflecting full subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a deeply held spiritual practice as mere 'theater' or 'extraction.' By framing study as a direct exercise of commitment, it avoids the mandatrophy trap where a practice persists without its original function. The function has been reinterpreted, not atrophied, ensuring the constraint remains a genuine expression of faith rather than an inertial ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_full_performance_ambiguity,
    'Is the intellectual study of sacrifice laws truly a full and equivalent performance of the divine command, or a temporary substitute for material performance?',
    'Theological consensus shifts, or the re-establishment of conditions for material sacrifice (e.g., rebuilding of the Temple).',
    'If resolved as a temporary substitute, the constraint''s ''mountain'' classification would be challenged, potentially reclassifying it as a ''scaffold'' or ''piton'' depending on the degree of perceived ''holding pattern'' vs. ''atrophied function.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_full_performance_ambiguity, conceptual, 'Ambiguity regarding the equivalence of study to material sacrifice.').

omega_variable(
    natural_law_vs_constructed_interpretation,
    'Is the ''study as exercise'' interpretation a genuine natural law of spiritual engagement, or a constructed theological framework that benefits the studying community?',
    'Cross-cultural theological comparison of how other traditions maintain commitment in the absence of material conditions, or internal doctrinal shifts within the tradition.',
    'If resolved as a constructed framework, the ''emerges_naturally: true'' claim would be challenged, potentially reclassifying the constraint as a ''rope'' or ''tangled_rope'' if elements of coordination or extraction are identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_interpretation, conceptual, 'Ambiguity between natural spiritual law and constructed theological interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.0).
narrative_ontology:measurement(temp_tr_t25, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 25, 0.0).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 50, 0.0).
narrative_ontology:measurement(temp_tr_t75, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 75, 0.0).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(temp_be_t25, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 25, 0.0).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 50, 0.0).
narrative_ontology:measurement(temp_be_t75, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 75, 0.0).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 100, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(temp_su_t25, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 25, 0.0).
narrative_ontology:measurement(temp_su_t50, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 50, 0.0).
narrative_ontology:measurement(temp_su_t75, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 75, 0.0).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
