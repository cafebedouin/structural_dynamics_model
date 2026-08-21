% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__composite_overdetermination_reading, []).

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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Reformation Event Boundary: Composite Overdetermination Reading
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the historiographical understanding of the
 *   Reformation as an overdetermined, composite event, where multiple causal
 *   factors (theological, political, institutional, social) operated
 *   simultaneously and irreducibly. It is a reading that resists reduction to
 *   a single primary cause or a linear periodization. This reading is
 *   presented as a 'mountain' because it asserts a structural feature of
 *   historical reality—the inherent complexity and overdetermination of major
 *   historical events—rather than a human-constructed constraint. Its
 *   'naturalness' is derived from the observed irreducibility of causal
 *   factors in historical analysis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.25).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.15).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation Event Boundary: Composite Overdetermination Reading").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:emerges_naturally(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '03534793-580c-4150-890b-c3553ed4b9ee').
narrative_ontology:cs_kernel_codification('03534793-580c-4150-890b-c3553ed4b9ee', distributed).
narrative_ontology:cs_authority_grounding('03534793-580c-4150-890b-c3553ed4b9ee', expertise).
narrative_ontology:cs_interpretation_layer_present('03534793-580c-4150-890b-c3553ed4b9ee').
narrative_ontology:cs_reading_relation('03534793-580c-4150-890b-c3553ed4b9ee', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('03534793-580c-4150-890b-c3553ed4b9ee', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('03534793-580c-4150-890b-c3553ed4b9ee', foundational, historical_causality_is_multi_layered).
narrative_ontology:cs_axiom_status(historical_causality_is_multi_layered, holdable).
narrative_ontology:cs_axiom_grounding('03534793-580c-4150-890b-c3553ed4b9ee', historical_causality_is_multi_layered, empirically_contingent).
narrative_ontology:cs_axiom('03534793-580c-4150-890b-c3553ed4b9ee', foundational, periodization_is_interpretive_not_absolute).
narrative_ontology:cs_axiom_status(periodization_is_interpretive_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('03534793-580c-4150-890b-c3553ed4b9ee', periodization_is_interpretive_not_absolute, conventional).
narrative_ontology:cs_reference_frame('03534793-580c-4150-890b-c3553ed4b9ee', post_annales_school_historiography).
narrative_ontology:cs_drift_state('03534793-580c-4150-890b-c3553ed4b9ee', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('03534793-580c-4150-890b-c3553ed4b9ee', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, historiographers_of_complexity).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, interdisciplinary_scholars).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, historical_contingency_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, emergent_complexity_in_history).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a reading that validates complex, multi-causal explanations over reductionist narratives. Their work is affirmed by the irreducible composite nature of the event.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historiographers_of_complexity, beneficiary,
    analytical, generational, analytical, global).

% Find this reading productive for integrating insights from theology, political science, sociology, and economics into a coherent understanding of the Reformation. It supports their methodological approach.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, interdisciplinary_scholars, beneficiary,
    analytical, biographical, analytical, global).

% Would object to the overdetermination reading as it diminishes the singular importance of theological innovation. Their interpretive framework prioritizes a single, primary cause.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, theological_reductionists, excluded,
    moderate, biographical, identity_locked, regional).

% Would object to the overdetermination reading as it dilutes the primacy of political and economic drivers. Their framework seeks a clear, materialist causal chain.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, political_reductionists, excluded,
    moderate, biographical, identity_locked, regional).

% Receives simplified narratives of the Reformation, often aligned with one of the reductionist readings. This complex reading is less accessible but offers a more nuanced understanding.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, general_public, observer,
    powerless, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for integrating diverse historical data and disciplinary perspectives into a coherent, non-reductive understanding of the Reformation, coordinating scholarly discourse around complexity.
% TRANSFER_FUNCTION: Transfers explanatory power from single-cause narratives to multi-causal, emergent explanations, shifting academic prestige and research funding towards interdisciplinary approaches.
% ABSENT_VOICES: Scholars committed to single-cause explanations (theological or political reductionists) are marginalized in this discourse, as their methodologies are deemed insufficient to capture the event's complexity.
% DISAPPEARANCE_RATIONALE: If the composite overdetermination reading vanished, historical scholarship on the Reformation would likely revert to more reductionist, single-cause explanations, leading to fragmented and less comprehensive understandings of the event's origins and consequences.
% FOUNDING_PROBLEM: The problem of adequately explaining the Reformation's origins and consequences without resorting to overly simplistic or ideologically driven single-cause narratives.
% FOUNDING_PROBLEM_CORROBORATION: Historians of various sub-disciplines (social, cultural, economic history) and philosophers of history corroborate the persistent challenge of integrating diverse causal factors without reductionism. This is attested by ongoing debates in historiographical journals and conferences, outside the immediate beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_event_boundary__composite_overdetermination_reading),
    narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because this reading primarily offers an analytical framework rather than directly extracting resources. Any 'extraction' is diffuse, in the form of intellectual labor required to engage with complexity. Suppression is low (0.15) as it doesn't actively coerce adherence, but rather gains acceptance through its explanatory power. Theater ratio is very low (0.05) as the reading is fundamentally about genuine intellectual engagement, not performance. Accessibility collapse is high (0.88) because once the irreducible complexity is understood, simpler narratives become intellectually untenable. Resistance is low (0.08) because while some scholars prefer simpler narratives, the evidence for complexity is robust.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between those who embrace historical complexity and those who seek singular, dominant causal explanations. This reading is a 'mountain' from the perspective of complexity scholars, reflecting an inherent truth about historical events. From the perspective of reductionists, it might be seen as a 'rope' or 'tangled rope' that coordinates academic discourse in a way that marginalizes their preferred methodologies.
 *
 * DIRECTIONALITY LOGIC:
 *   Historiographers of complexity and interdisciplinary scholars are beneficiaries (d near 0.0) as this reading validates their methodological approaches and provides a rich field for research. Reductionist scholars (theological or political) are structurally excluded (d near 1.0) because their frameworks are challenged by this reading, though they are not directly 'victims' in a material sense. The general public is an observer, largely unaffected directly by this academic debate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_complexity,
    'Is the ''overdetermined composite'' nature of the Reformation an inherent feature of historical reality (a Mountain), or a constructed historiographical framework (a Rope) that benefits certain academic approaches?',
    'Analysis of historical events of similar scale across different cultures and periods: if irreducible complexity consistently emerges, it supports the ''natural'' claim. If it is primarily a feature of Western historiography, it suggests a constructed framework.',
    'If constructed, the constraint would reclassify from Mountain to Rope, indicating a coordination mechanism for academic discourse rather than an inherent truth about the event itself. This would imply a higher, albeit still low, extractiveness for those who do not conform to the framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_complexity, conceptual, 'Ambiguity between inherent historical complexity and a constructed academic lens.').

omega_variable(
    periodization_consensus,
    'Can a universally accepted periodization of the Reformation be established, or is its contested nature an irreducible consequence of its composite character?',
    'Longitudinal study of historiographical trends: if a dominant periodization eventually emerges across diverse schools, it challenges the ''irreducible contestation'' claim. If contestation persists, it supports this reading.',
    'If a consensus periodization is possible, it would weaken the claim of ''overdetermination'' and suggest that the event''s boundaries are more amenable to singular definition, potentially shifting the reading closer to a ''rope'' that coordinates a shared timeline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(periodization_consensus, empirical, 'Whether the contested periodization is a structural feature or a solvable problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1950, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1950, 0.02).
narrative_ontology:measurement(refo_tr_t1970, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1970, 0.03).
narrative_ontology:measurement(refo_tr_t1990, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(refo_tr_t2010, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(refo_tr_t2024, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(refo_be_t1950, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(refo_be_t1970, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(refo_be_t1990, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1990, 0.23).
narrative_ontology:measurement(refo_be_t2010, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(refo_be_t2024, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1950, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(refo_su_t1970, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1970, 0.12).
narrative_ontology:measurement(refo_su_t1990, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1990, 0.13).
narrative_ontology:measurement(refo_su_t2010, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement(refo_su_t2024, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
