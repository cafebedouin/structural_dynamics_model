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
    narrative_ontology:affects_constraint/2,
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
 *   This constraint describes the interpretive tradition within Halakhic
 *   Judaism that views the intellectual study of the laws of temple sacrifice
 *   as a form of actual performance of the divine command, particularly in
 *   the absence of the Temple. It is a reading of the broader
 *   'temple_sacrifice_commitment' kernel, asserting that intellectual
 *   engagement is a valid and intrinsically valuable exercise of covenantal
 *   fidelity. The constraint is claimed as a Mountain because, within this
 *   interpretive framework, the value and efficacy of study as performance
 *   are considered an unchangeable spiritual truth, not a human construct or
 *   extractive mechanism. It is a structural feature of how commitment is
 *   understood and maintained.
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
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Temple Sacrifice Commitment: Study as Exercise").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, 'f18cbfda-3916-4a41-bf1e-026d2fbf74de').
narrative_ontology:cs_kernel_codification('f18cbfda-3916-4a41-bf1e-026d2fbf74de', fixed_text).
narrative_ontology:cs_authority_grounding('f18cbfda-3916-4a41-bf1e-026d2fbf74de', lineage).
narrative_ontology:cs_interpretation_layer_present('f18cbfda-3916-4a41-bf1e-026d2fbf74de').
narrative_ontology:cs_reading_relation('f18cbfda-3916-4a41-bf1e-026d2fbf74de', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('f18cbfda-3916-4a41-bf1e-026d2fbf74de', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('f18cbfda-3916-4a41-bf1e-026d2fbf74de', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('f18cbfda-3916-4a41-bf1e-026d2fbf74de', foundational, intellectual_engagement_is_performance).
narrative_ontology:cs_axiom_status(intellectual_engagement_is_performance, holdable).
narrative_ontology:cs_axiom_grounding('f18cbfda-3916-4a41-bf1e-026d2fbf74de', intellectual_engagement_is_performance, deontological).
narrative_ontology:cs_reference_frame('f18cbfda-3916-4a41-bf1e-026d2fbf74de', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('f18cbfda-3916-4a41-bf1e-026d2fbf74de', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f18cbfda-3916-4a41-bf1e-026d2fbf74de', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community of scholars and adherents who engage in the intellectual study of the laws of temple sacrifice. They derive spiritual and communal benefit from this engagement, seeing it as a direct fulfillment of divine command and a means of maintaining covenant fidelity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, generational, identity_locked, global).

% The abstract concept of faithfulness to the divine covenant. This reading asserts that intellectual engagement with the laws of sacrifice directly contributes to and embodies this fidelity, even in the absence of material conditions for performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity).

% The ultimate source of the laws of temple sacrifice. This reading interprets the divine will as encompassing intellectual engagement as a form of performance, thereby maintaining the command's live status.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, divine_command, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, divine_command).

% Those who believe that divine command for sacrifice requires material performance and that study alone is insufficient to occupy the commitment. They are excluded from the interpretive framework of this reading, which redefines 'performance'.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, performance_only_adherents, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing spiritual and intellectual engagement of a dispersed community with a central, historically rooted divine command, ensuring its continued relevance and active observance through study.
% TRANSFER_FUNCTION: Transfers spiritual merit and communal cohesion to the studying community, and maintains the 'live' status of the divine command, in exchange for intellectual effort and interpretive fidelity.
% ABSENT_VOICES: Adherents of the 'performance_only' reading are absent from this interpretive framework; they would argue that study is merely archival and does not constitute active performance of the divine command, thus diminishing the commitment.
% DISAPPEARANCE_RATIONALE: If this interpretive constraint vanished, the studying community would lose a primary mode of covenantal engagement, potentially leading to a decline in the study of sacrifice law and a shift in the perceived 'live' status of the divine command. The community's identity and spiritual practice would need to fundamentally reorganize.
% FOUNDING_PROBLEM: The historical destruction of the Temple and the cessation of material sacrifices, which left a central divine command without its prescribed mode of performance.
% FOUNDING_PROBLEM_CORROBORATION: The problem of how to fulfill the divine command for sacrifice in the absence of the Temple is universally acknowledged within the tradition. The 'study_as_exercise' reading is a widely accepted, though not universally exclusive, solution attested by centuries of rabbinic scholarship and communal practice, not just by its beneficiaries.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).

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
 *   The extractiveness is 0.0 because study is considered an intrinsically valuable act of devotion, not a burden or a means of extraction. Suppression is 0.0 as there is no coercion to study; it is a voluntary act of spiritual engagement. Theater ratio is 0.0 because the study is understood as genuine performance, not a substitute or a mere show. Accessibility collapse is high (0.95) because, within this framework, the 'alternative' of not engaging with the divine command is spiritually untenable for the committed, and study provides a universally accessible path. Resistance is low (0.05) because, while other readings exist, this one is widely accepted and practiced, facing little internal resistance from its adherents.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap within this reading, as all participants (the studying community, the concept of covenant fidelity) share the understanding that study is a valid and beneficial form of performance. The 'gap' exists between this reading and other readings of the kernel, which is handled by omega variables and network links.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community and covenant fidelity are beneficiaries (d=0.0) as they directly gain spiritual and communal value from this interpretation. The divine command is the agenda-setter (d=0.0) as it is the source of the commitment. There are no victims within this specific reading, as no one is coerced or extracted from by the act of study itself. Those who hold alternative readings are 'excluded' from this interpretive frame, but not 'victims' of it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively resolves the mandatrophy that would otherwise arise from the destruction of the Temple. By reinterpreting 'performance' to include study, it ensures the divine command remains 'live' and actively engaged with, preventing the commitment from atrophying into a mere historical archive. It prevents the mislabeling of a vital spiritual practice as a 'piton' or 'snare' by asserting its intrinsic value and direct fulfillment of the mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_legitimacy_of_study,
    'Is the interpretation of ''study as performance'' a legitimate fulfillment of the divine command, or a rabbinic innovation to cope with historical circumstances?',
    'Deep textual analysis of early rabbinic sources and theological arguments regarding the nature of divine command and human obligation. Consensus among leading halakhic authorities.',
    'If deemed a mere innovation, the ''mountain'' claim of intrinsic value would weaken, potentially shifting towards a ''scaffold'' (temporary coping mechanism) or ''tangled_rope'' (maintaining community at interpretive cost). If affirmed as legitimate, the mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_legitimacy_of_study, conceptual, 'The conceptual legitimacy of study as a form of divine command fulfillment.').

omega_variable(
    relationship_to_material_performance,
    'Does ''study as exercise'' fully occupy the commitment, or does it leave a residual obligation for material performance if conditions allow?',
    'Analysis of halakhic rulings on the messianic era and the rebuilding of the Temple: would study continue to be considered ''performance'' in the presence of a rebuilt Temple, or would material sacrifice resume primacy?',
    'If study fully occupies the commitment, its mountain status is reinforced. If it is a placeholder for future material performance, it leans towards ''scaffold'' or ''hybrid_preparatory'' (a sibling reading), implying a temporary rather than ultimate fulfillment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relationship_to_material_performance, conceptual, 'Whether study is a complete or partial fulfillment of the sacrifice commitment.').

omega_variable(
    false_summit_ambiguity,
    'Is this constraint a genuine natural law of spiritual engagement, or a constructed interpretive framework that benefits identifiable agents (the studying community) by maintaining their relevance?',
    'Examination of the historical development of the ''study as performance'' doctrine: did it emerge organically from theological principles, or was it a strategic response to institutional challenges and the need to maintain communal cohesion and rabbinic authority post-Temple destruction?',
    'If primarily a strategic construction, the ''mountain'' claim is a false summit, and the constraint would reclassify as a ''tangled_rope'' (coordinating community around a beneficial interpretation, but with an unacknowledged constructed element). If genuinely emergent from theological principles, the mountain holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_ambiguity, empirical, 'Ambiguity between genuine spiritual law and beneficial interpretive construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of multiple readings of the 'temple_sacrifice_commitment' kernel. Each reading represents a distinct structural claim about how the divine command for sacrifice is fulfilled in the absence of the Temple.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
