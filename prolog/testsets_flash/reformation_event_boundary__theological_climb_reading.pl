% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation as Theological Breakthrough (Theological Climb Reading)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint models the Reformation as a genuine theological
 *   innovation event, where Luther's rediscovery of 'justification by faith
 *   alone' constituted a doctrinal breakthrough. This breakthrough, rooted in
 *   a new reading of scripture, inherently required institutional separation
 *   from the Roman Catholic Church. The periodization is tight (1517-1555),
 *   focusing on the initial theological challenge and its immediate
 *   institutional consequences. The Catholic Church hierarchy is framed as a
 *   victim of this theological correction, while Protestant believers and
 *   theologians are beneficiaries of the 'climb' to a more accurate
 *   understanding of faith.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.15).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.2).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Breakthrough (Theological Climb Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '1da6940e-1750-47cc-9dd3-e75d4928b780').
narrative_ontology:cs_kernel_codification('1da6940e-1750-47cc-9dd3-e75d4928b780', fixed_text).
narrative_ontology:cs_authority_grounding('1da6940e-1750-47cc-9dd3-e75d4928b780', lineage).
narrative_ontology:cs_interpretation_layer_present('1da6940e-1750-47cc-9dd3-e75d4928b780').
narrative_ontology:cs_reading_relation('1da6940e-1750-47cc-9dd3-e75d4928b780', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('1da6940e-1750-47cc-9dd3-e75d4928b780', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('1da6940e-1750-47cc-9dd3-e75d4928b780', foundational, sola_fide_is_scriptural_truth).
narrative_ontology:cs_axiom_status(sola_fide_is_scriptural_truth, holdable).
narrative_ontology:cs_axiom_grounding('1da6940e-1750-47cc-9dd3-e75d4928b780', sola_fide_is_scriptural_truth, deontological).
narrative_ontology:cs_axiom('1da6940e-1750-47cc-9dd3-e75d4928b780', secondary, papal_authority_is_usurpation).
narrative_ontology:cs_axiom_status(papal_authority_is_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('1da6940e-1750-47cc-9dd3-e75d4928b780', papal_authority_is_usurpation, theological).
narrative_ontology:cs_reference_frame('1da6940e-1750-47cc-9dd3-e75d4928b780', pristine_apostolic_doctrine).
narrative_ontology:cs_drift_state('1da6940e-1750-47cc-9dd3-e75d4928b780', contemporary_ecumenical_dialogue, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('1da6940e-1750-47cc-9dd3-e75d4928b780', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_theologians).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, roman_catholic_church_hierarchy).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_fide_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_scriptura_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary agent of theological innovation, whose rediscovery of justification by faith alone initiated the doctrinal shift. His commitment to scriptural truth made institutional separation a necessary consequence.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, martin_luther, agenda_setter,
    powerful, generational, identity_locked, continental).

% Benefited from the perceived liberation from a works-based salvation system and direct access to scripture. They experienced a spiritual 'climb' towards a more authentic faith.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, protestant_believers, beneficiary,
    organized, generational, mobile, continental).

% Developed and systematized the new theological insights, establishing new academic and ecclesiastical traditions. Their careers and intellectual frameworks were built upon this doctrinal breakthrough.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformed_theologians, beneficiary,
    institutional, generational, constrained, continental).

% Suffered a loss of spiritual authority, territorial control, and financial resources as a direct consequence of the theological challenge. They were the target of doctrinal correction and institutional separation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, roman_catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, global).

% While often acting for political gain, from this reading's perspective, they were secondary actors whose political decisions enabled the theological separation, rather than driving it. They observed and reacted to the doctrinal imperative.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers, observer,
    institutional, generational, arbitrage, regional).

% Analyze the theological arguments and their historical impact, seeking to understand the genuine doctrinal innovations and their necessity for institutional change.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Re-coordinated Christian doctrine around a new understanding of salvation, providing a coherent theological framework for believers and clergy who adopted it.
% TRANSFER_FUNCTION: Transferred spiritual authority from the Roman Catholic hierarchy to individual conscience and scripture, and transferred allegiance of believers from the Pope to reformed confessions.
% ABSENT_VOICES: Those who prioritized institutional unity above doctrinal purity, or those who saw the theological disputes as secondary to political or economic grievances, would argue that the 'breakthrough' was not worth the schism.
% DISAPPEARANCE_RATIONALE: If the theological breakthrough and its institutional consequences vanished, the entire landscape of Western Christianity, political structures, and intellectual history would be fundamentally different. The Protestant denominations, their cultural impact, and the Catholic Counter-Reformation would not exist.
% FOUNDING_PROBLEM: The Roman Catholic Church's doctrine of salvation by works, sale of indulgences, and perceived corruption obscured the true path to salvation, creating spiritual anxiety and a false understanding of grace.
% FOUNDING_PROBLEM_CORROBORATION: Protestant theologians and historians continue to attest to the live nature of the theological problem, viewing the Reformation as an ongoing recovery of essential Christian truths. While Catholic scholars dispute the 'problem' as framed, the theological differences remain central to denominational identity, corroborated by centuries of doctrinal debate and separate ecclesiastical structures.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because, from this reading's perspective, the theological truth of 'sola fide' is an unchangeable spiritual reality that Luther merely 'rediscovered.' The low extractiveness (0.15) and suppression (0.2) reflect that the 'truth' itself is not extractive, and its acceptance, while initially resisted, ultimately emerged as a self-evident spiritual necessity for its adherents. The low theater ratio (0.05) indicates minimal performative maintenance; the theological claims were genuinely believed and acted upon. The high accessibility collapse (0.88) reflects that once the 'truth' of justification by faith alone was understood, alternatives (works-based salvation) collapsed for those who accepted it. Resistance (0.1) was low from the perspective of the 'truth' itself, though high from the perspective of the institutional response.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Roman Catholic Church hierarchy, this 'theological climb' was a destructive heresy, not a breakthrough. However, this reading focuses on the internal logic and perceived necessity of the theological innovation itself, where the 'truth' of the doctrine compelled a new institutional form. The engine's classification will reflect the low extraction inherent to a 'rediscovered truth,' even if it caused immense institutional disruption.
 *
 * DIRECTIONALITY LOGIC:
 *   Martin Luther and reformed theologians are agenda-setters and beneficiaries, as they articulated and propagated the 'new' truth. Protestant believers are beneficiaries, experiencing spiritual liberation. The Roman Catholic Church hierarchy is the primary victim, as its authority and doctrines were directly challenged and diminished. Secular rulers are observers, whose political actions facilitated the theological shift but did not define its core nature.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the theological imperative as mere political opportunism or institutional power grab. By framing it as a genuine doctrinal breakthrough, it highlights the internal, epistemic drivers of the Reformation, rather than reducing it to external factors. The 'mandate' was the rediscovery of a fundamental theological truth, which, from this perspective, remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_causation,
    'To what extent was Luther''s theological breakthrough the primary driver of the Reformation, versus political and economic factors that exploited the theological disputes?',
    'Detailed historical analysis of primary sources, focusing on the motivations of key actors and the sequencing of events. Counterfactual history exploring how the Reformation might have unfolded without Luther''s specific theological insights.',
    'If political factors are found to be dominant, the ''theological_climb_reading'' would be reclassified as a ''snare'' or ''tangled_rope'' where theological claims served as cover for extraction. If theological factors remain primary, the ''mountain'' classification holds for the doctrinal core.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_vs_political_causation, empirical, 'Ambiguity regarding the primary causal driver of the Reformation: theological vs. political/economic.').

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is ''justification by faith alone'' a natural, immutable theological truth (a spiritual ''natural law''), or a constructed doctrinal interpretation that gained ascendancy?',
    'Comparative theological analysis across diverse Christian traditions and historical periods. Examination of the hermeneutical principles used to derive the doctrine and their historical contingency.',
    'If it is a constructed doctrine, the ''mountain'' classification is challenged, potentially reclassifying it as a ''rope'' or ''tangled_rope'' that coordinated a specific theological community but was not an unchangeable truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether the core theological claim is a natural law or a constructed interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.03).
narrative_ontology:measurement(refo_tr_t1525, reformation_event_boundary__theological_climb_reading, theater_ratio, 1525, 0.04).
narrative_ontology:measurement(refo_tr_t1535, reformation_event_boundary__theological_climb_reading, theater_ratio, 1535, 0.04).
narrative_ontology:measurement(refo_tr_t1545, reformation_event_boundary__theological_climb_reading, theater_ratio, 1545, 0.05).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.05).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.1).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1525, 0.12).
narrative_ontology:measurement(refo_be_t1535, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1535, 0.13).
narrative_ontology:measurement(refo_be_t1545, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1545, 0.14).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.15).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.17).
narrative_ontology:measurement(refo_su_t1535, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1535, 0.18).
narrative_ontology:measurement(refo_su_t1545, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1545, 0.19).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, protestant_work_ethic_norm).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, modern_state_sovereignty_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reformation_event_boundary' kernel, focusing on the theological innovation aspect. It is linked to sibling readings that emphasize political or composite causes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
