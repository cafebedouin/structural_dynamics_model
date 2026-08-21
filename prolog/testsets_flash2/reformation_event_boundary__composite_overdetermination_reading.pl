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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Reformation as Composite Overdetermined Event
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the historiographical reading of the
 *   Reformation as an overdetermined, composite event, where theological,
 *   institutional, and political factors converged irreducibly. It challenges
 *   reductionist accounts by asserting the simultaneous and intertwined
 *   nature of these drivers. This is one reading of the
 *   'reformation_event_boundary' kernel, emphasizing complexity over singular
 *   causality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.2).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.1).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation as Composite Overdetermined Event").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:emerges_naturally(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '868ac65e-d905-4b41-80f8-b453ae070668').
narrative_ontology:cs_kernel_codification('868ac65e-d905-4b41-80f8-b453ae070668', distributed).
narrative_ontology:cs_authority_grounding('868ac65e-d905-4b41-80f8-b453ae070668', expertise).
narrative_ontology:cs_interpretation_layer_present('868ac65e-d905-4b41-80f8-b453ae070668').
narrative_ontology:cs_reading_relation('868ac65e-d905-4b41-80f8-b453ae070668', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('868ac65e-d905-4b41-80f8-b453ae070668', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('868ac65e-d905-4b41-80f8-b453ae070668', foundational, historical_causality_is_irreducibly_plural).
narrative_ontology:cs_axiom_status(historical_causality_is_irreducibly_plural, holdable).
narrative_ontology:cs_axiom_grounding('868ac65e-d905-4b41-80f8-b453ae070668', historical_causality_is_irreducibly_plural, empirically_contingent).
narrative_ontology:cs_axiom('868ac65e-d905-4b41-80f8-b453ae070668', secondary, periodization_is_an_interpretive_act).
narrative_ontology:cs_axiom_status(periodization_is_an_interpretive_act, holdable).
narrative_ontology:cs_axiom_grounding('868ac65e-d905-4b41-80f8-b453ae070668', periodization_is_an_interpretive_act, conventional).
narrative_ontology:cs_reference_frame('868ac65e-d905-4b41-80f8-b453ae070668', post_modern_historiographical_pluralism).
narrative_ontology:cs_drift_state('868ac65e-d905-4b41-80f8-b453ae070668', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('868ac65e-d905-4b41-80f8-b453ae070668', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, historiographers_of_complexity).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, post_denominational_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, theological_reductionists).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, political_reductionists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a reading that validates complex, multi-causal historical analysis, resisting reductionist narratives. Their careers are built on demonstrating the irreducible nature of historical phenomena.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historiographers_of_complexity, beneficiary,
    analytical, generational, analytical, global).

% Benefit from a reading that de-centers any single theological or political origin, allowing for a more fluid and less polemical understanding of Christian history and ecumenical dialogue.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, post_denominational_theologians, beneficiary,
    analytical, generational, analytical, global).

% Bear the cost of having their preferred single-cause narratives (e.g., 'it was all about Luther's theology') challenged and undermined by a more complex, overdetermined account. Their interpretive frameworks are destabilized.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, theological_reductionists, payer,
    analytical, generational, constrained, global).

% Similarly bear the cost of having their preferred single-cause narratives (e.g., 'it was all about power politics') challenged by a multi-causal account. Their materialist interpretations are seen as incomplete.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, political_reductionists, payer,
    analytical, generational, constrained, global).

% Are implicitly challenged by a reading that denies a single, clear beginning or end point for the Reformation, making neat chronological divisions problematic. They would prefer a simpler, more linear narrative.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historical_periodization_schemes, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(reformation_event_boundary__composite_overdetermination_reading, historical_periodization_schemes).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates historical understanding by providing a framework that integrates diverse causal factors (theological, political, institutional, social) into a coherent, albeit complex, narrative of the Reformation.
% TRANSFER_FUNCTION: Transfers interpretive authority from single-cause explanations to multi-causal, overdetermined ones, shifting academic prestige and research focus towards interdisciplinary approaches.
% ABSENT_VOICES: Scholars committed to a single, dominant causal factor for the Reformation (e.g., purely theological or purely political) are implicitly excluded from the 'composite' framing, as their methodologies are deemed insufficient to capture the phenomenon's full complexity.
% DISAPPEARANCE_RATIONALE: If this reading vanished, historical scholarship on the Reformation would likely revert to more reductionist, single-cause explanations, leading to renewed historiographical disputes and a less nuanced understanding of the period's complexity.
% FOUNDING_PROBLEM: The problem of reconciling diverse, seemingly contradictory historical accounts of the Reformation into a single, coherent narrative without privileging one causal factor over others.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by ongoing debates in historical and theological scholarship, where new evidence and interpretive frameworks continue to challenge simplistic narratives. The problem is attested by the persistent failure of any single-cause theory to achieve universal acceptance among historians.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low because this reading primarily extracts from other analytical frameworks, not from material actors. Suppression is also low, as it operates within academic discourse, where alternative readings are not actively suppressed but rather engaged and critiqued. The 'emerges_naturally: true' reflects the analytical claim that the composite nature of the Reformation is an inherent feature of the historical record, not a constructed interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (historiographers of complexity) experience this as a clarifying and enabling framework, while the payers (theological and political reductionists) experience it as a challenge to their established interpretive models. The constraint itself is an analytical tool, so its 'extraction' is primarily intellectual and methodological.
 *
 * DIRECTIONALITY LOGIC:
 *   Historiographers and theologians who embrace complexity are beneficiaries, as this reading validates their approach. Those committed to single-cause explanations are payers, as their frameworks are challenged. The constraint itself, as an analytical framework, is not actively enforced in a coercive sense but rather gains traction through its explanatory power and academic consensus.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the mislabeling of complex historical phenomena as simple, single-cause events. It ensures that the 'mandate' of historical inquiry—to accurately represent the past—is not atrophied by oversimplification. By asserting overdetermination, it guards against the temptation to reduce the Reformation to a 'snare' of political power or a 'rope' of theological consensus, instead presenting it as a 'mountain' of irreducible historical complexity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historiographical_consensus_stability,
    'Will the ''composite overdetermination'' reading achieve lasting consensus, or will new evidence/interpretations lead to a re-fragmentation of Reformation historiography?',
    'Longitudinal study of academic publications and major historical syntheses over the next 50 years; shifts in university curriculum and research funding priorities.',
    'If consensus holds, the reading solidifies as a ''mountain'' of historical understanding. If it fragments, its ''mountain'' status might degrade to a ''piton'' of a past academic trend, or a ''tangled_rope'' of ongoing, unresolved debate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historiographical_consensus_stability, empirical, 'The stability of the composite reading within academic discourse.').

omega_variable(
    natural_vs_constructed_complexity,
    'Is the ''overdetermined composite'' nature of the Reformation an inherent feature of the historical event itself (a natural law), or is it a constructed analytical framework imposed by modern historiography?',
    'Analysis of primary sources for explicit contemporary recognition of multi-causality vs. later interpretive overlays. This is a conceptual distinction that may not be empirically resolvable.',
    'If truly natural, its ''mountain'' status is robust. If constructed, it might be reclassified as a ''rope'' (a useful coordination device) or even a ''tangled_rope'' (if it implicitly extracts from simpler narratives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_complexity, conceptual, 'Whether the composite nature is intrinsic to the event or an interpretive construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(refo_be_t1950, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(refo_be_t1970, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(refo_be_t1990, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(refo_be_t2010, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2010, 0.2).
narrative_ontology:measurement(refo_be_t2024, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1950, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(refo_su_t1970, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement(refo_su_t1990, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(refo_su_t2010, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(refo_su_t2024, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, information_standard).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, political_swap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'reformation_event_boundary' kernel, each representing a distinct historiographical interpretation. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
