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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Reformation as Composite Overdetermined Event
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint models the historiographical claim that the Reformation
 *   was an overdetermined composite event, irreducible to a single causal
 *   driver. It integrates theological, institutional, and political
 *   dimensions as simultaneously operative. This reading is presented as a
 *   'mountain' because it asserts a structural feature of the historical
 *   event itself—its inherent complexity and overdetermination—rather than a
 *   human-constructed rule. However, it declares beneficiaries (pluralist and
 *   critical historians) and victims (reductionist and confessional
 *   historians) because the *acceptance* of this reading within the academic
 *   discourse confers advantages and disadvantages, triggering False Summit
 *   Mountain analysis. The metrics reflect the low inherent extraction of a
 *   'natural' historical truth, but also the subtle 'suppression' of simpler
 *   narratives and the 'resistance' from those who prefer them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.3).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.2).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation as Composite Overdetermined Event").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:emerges_naturally(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '543ae6c1-4b61-4731-8462-137cb9f67126').
narrative_ontology:cs_kernel_codification('543ae6c1-4b61-4731-8462-137cb9f67126', distributed).
narrative_ontology:cs_authority_grounding('543ae6c1-4b61-4731-8462-137cb9f67126', expertise).
narrative_ontology:cs_interpretation_layer_present('543ae6c1-4b61-4731-8462-137cb9f67126').
narrative_ontology:cs_reading_relation('543ae6c1-4b61-4731-8462-137cb9f67126', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('543ae6c1-4b61-4731-8462-137cb9f67126', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('543ae6c1-4b61-4731-8462-137cb9f67126', foundational, historical_events_are_irreducibly_multi_causal).
narrative_ontology:cs_axiom_status(historical_events_are_irreducibly_multi_causal, holdable).
narrative_ontology:cs_axiom_grounding('543ae6c1-4b61-4731-8462-137cb9f67126', historical_events_are_irreducibly_multi_causal, empirically_contingent).
narrative_ontology:cs_axiom('543ae6c1-4b61-4731-8462-137cb9f67126', foundational, no_single_periodization_is_universally_valid).
narrative_ontology:cs_axiom_status(no_single_periodization_is_universally_valid, holdable).
narrative_ontology:cs_axiom_grounding('543ae6c1-4b61-4731-8462-137cb9f67126', no_single_periodization_is_universally_valid, empirically_contingent).
narrative_ontology:cs_reference_frame('543ae6c1-4b61-4731-8462-137cb9f67126', post_annales_school_historiography).
narrative_ontology:cs_drift_state('543ae6c1-4b61-4731-8462-137cb9f67126', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('543ae6c1-4b61-4731-8462-137cb9f67126', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, pluralist_historiographers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, critical_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, reductionist_historians).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, confessional_historians).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, historical_contingency_principle).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, multi_causal_explanation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a reading that validates complex, multi-causal historical explanations, reinforcing their methodological commitments against reductionist narratives. Their careers are built on synthesizing diverse historical forces.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, pluralist_historiographers, beneficiary,
    institutional, generational, analytical, global).

% Benefit from a reading that acknowledges the irreducible theological dimension of the Reformation while integrating its political and social contexts, preventing a purely secular or reductionist interpretation of religious change.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, critical_theologians, beneficiary,
    organized, generational, analytical, global).

% Find their preferred single-cause or linear-progression narratives challenged by this composite reading, requiring them to revise their explanatory models or defend against charges of oversimplification. This reading imposes a cost on their preferred methodological simplicity.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, reductionist_historians, payer,
    powerful, biographical, constrained, global).

% Often tied to specific denominational narratives that emphasize a particular causal driver (e.g., purely theological or purely political). This reading complicates their efforts to present a unified, teleological account of the Reformation, forcing them to confront irreducible ambiguities.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, confessional_historians, payer,
    organized, generational, identity_locked, global).

% Analyze the structural properties of historical explanation itself, using the Reformation as a case study for understanding concepts like overdetermination, causality, and periodization. They are not directly affected by the historical outcome but by the clarity of its conceptualization.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, analytical_philosophers_of_history, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for integrating diverse historical data and interpretive lenses into a coherent, albeit complex, understanding of the Reformation, preventing fragmentation into isolated disciplinary silos.
% TRANSFER_FUNCTION: Transfers explanatory power from single-cause narratives to multi-causal, irreducible accounts, shifting academic prestige and research funding towards interdisciplinary and complex historical methodologies.
% ABSENT_VOICES: Historians committed to a single, dominant causal narrative (e.g., purely economic, purely theological, purely political) are implicitly excluded from the full explanatory scope of this reading; they would argue for the primacy of their chosen factor.
% DISAPPEARANCE_RATIONALE: If this composite reading vanished, historical scholarship on the Reformation would likely revert to more reductionist, disciplinary-bound explanations, losing the synthetic power and nuance that acknowledges the event's irreducible complexity. The field would fragment.
% FOUNDING_PROBLEM: The problem of reconciling diverse, often conflicting, historical accounts and disciplinary perspectives on the Reformation into a single, coherent, yet non-reductive explanatory framework.
% FOUNDING_PROBLEM_CORROBORATION: Historians across multiple sub-disciplines (social, political, intellectual, theological history) attest to the ongoing challenge of integrating these perspectives without oversimplification. Peer-reviewed journals and academic conferences consistently feature debates on the 'causes' and 'nature' of the Reformation, corroborating the problem's live status from outside any single benefiting party.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

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
 *   The low extractiveness (0.3) and suppression (0.2) reflect the idea that the composite nature of the Reformation is an inherent feature of the historical record, not a human-imposed rule. However, the non-zero values acknowledge that advocating for this complex view requires intellectual effort and challenges simpler, more 'clean' narratives, thus imposing a subtle cost on those who prefer reductionism. The 'resistance' (0.4) comes from the ongoing academic debates where single-cause theories are still defended. The 'accessibility_collapse' (0.7) is high because once the irreducible complexity is understood, reverting to simplistic explanations becomes intellectually difficult.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pluralist historians, this reading is a natural and accurate reflection of historical reality. From the perspective of reductionist historians, it is an overly complex or even 'unscientific' approach that obscures clear causal lines. The engine's FSM analysis will evaluate whether this 'natural' claim is genuinely a mountain or a constructed framework benefiting specific academic factions.
 *
 * DIRECTIONALITY LOGIC:
 *   Pluralist and critical historians are beneficiaries (d near 0.0) as this reading validates their methodological approaches. Reductionist and confessional historians are payers (d near 1.0) because it challenges their preferred frameworks. Analytical philosophers of history are observers (d near 0.5) as they analyze the structure of the debate itself without direct stake in the historical outcome.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_complexity,
    'Is the ''composite overdetermination'' of the Reformation an inherent feature of the historical event (a Mountain), or a constructed historiographical framework that benefits certain academic approaches (a Tangled Rope)?',
    'Analysis of historical evidence for irreducible causal entanglement vs. evidence for methodological choices driving the ''composite'' interpretation. If the evidence for entanglement is overwhelming and independent of interpretive school, it leans Mountain. If the ''composite'' view is primarily a product of a specific academic paradigm, it leans Tangled Rope.',
    'If a genuine Mountain, the classification holds. If a constructed Tangled Rope, the effective extraction for reductionist historians would be higher, and the constraint would require active enforcement to maintain its dominance in academic discourse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_complexity, conceptual, 'Ambiguity between inherent historical complexity and historiographical construction.').

omega_variable(
    periodization_contestation,
    'Does the ''composite overdetermination'' reading adequately account for the ongoing contestation over the Reformation''s periodization (e.g., when it ''ended'')?',
    'Comparative analysis of periodization schemes across different historiographical schools. If the composite reading provides a robust framework for understanding why periodization remains contested, it is strengthened. If it merely sidesteps the issue, its explanatory power is weakened.',
    'If the composite reading fails to explain periodization contestation, its claim to comprehensive overdetermination is weakened, potentially increasing its ''resistance'' metric. If it successfully explains it, its ''accessibility_collapse'' for alternative views would increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_contestation, empirical, 'Impact of composite reading on periodization debates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1950, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(refo_tr_t1970, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(refo_tr_t1990, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(refo_tr_t2020, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(refo_be_t1950, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(refo_be_t1970, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(refo_be_t1990, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(refo_be_t2020, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2020, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1950, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(refo_su_t1970, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(refo_su_t1990, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1990, 0.19).
narrative_ontology:measurement(refo_su_t2020, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2020, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, information_standard).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__political_swap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reformation_event_boundary' kernel, emphasizing its composite and overdetermined nature. It contrasts with readings that prioritize theological or political causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
