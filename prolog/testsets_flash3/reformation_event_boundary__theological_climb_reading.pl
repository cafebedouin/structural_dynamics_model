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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation as Theological Breakthrough (Theological Climb Reading)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'theological_climb_reading' of the
 *   'reformation_event_boundary' kernel. It posits the Reformation as
 *   primarily a theological innovation, where Luther's rediscovery of
 *   justification by faith alone constituted a genuine doctrinal
 *   breakthrough. This breakthrough, seen as a 'climb' to a higher
 *   theological truth, inherently required institutional separation from the
 *   Roman Catholic Church, which is framed as having fallen into doctrinal
 *   error. The periodization is tight (1517-1555), focusing on the initial
 *   theological challenge and its immediate institutional consequences. The
 *   Catholic Church is cast as a victim of necessary theological correction,
 *   while Protestant believers and reformed theology are the beneficiaries of
 *   this 'climb'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.15).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.25).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Breakthrough (Theological Climb Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42').
narrative_ontology:cs_kernel_codification('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', fixed_text).
narrative_ontology:cs_authority_grounding('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', lineage).
narrative_ontology:cs_interpretation_layer_present('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42').
narrative_ontology:cs_reading_relation('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', foundational, justification_by_faith_alone_is_scriptural_truth).
narrative_ontology:cs_axiom_status(justification_by_faith_alone_is_scriptural_truth, holdable).
narrative_ontology:cs_axiom_grounding('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', justification_by_faith_alone_is_scriptural_truth, theological).
narrative_ontology:cs_axiom('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', foundational, scripture_alone_is_ultimate_authority).
narrative_ontology:cs_axiom_status(scripture_alone_is_ultimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', scripture_alone_is_ultimate_authority, theological).
narrative_ontology:cs_reference_frame('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', lutheran_theological_paradigm).
narrative_ontology:cs_drift_state('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', contemporary_ecumenical_dialogue, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('2f3bbcad-5c0d-4203-81f4-d93b6c2e1b42', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_theology).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, roman_catholic_church).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, secular_rulers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from a perceived liberation from a corrupt and theologically erroneous system, gaining direct access to scripture and a new understanding of salvation. Their identity became fused with the reformed doctrine.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, protestant_believers, beneficiary,
    organized, generational, identity_locked, continental).

% Suffered a loss of authority, membership, and assets as a result of the theological challenge. From this reading's perspective, the Church was the target of necessary theological correction, forced to confront its doctrinal errors.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, roman_catholic_church, payer,
    institutional, civilizational, constrained, global).

% The primary agent of the theological breakthrough, whose 'rediscovery' of justification by faith alone initiated the doctrinal shift. His commitment to this theological truth made institutional separation inevitable.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, martin_luther, agenda_setter,
    powerful, biographical, identity_locked, regional).

% While benefiting politically from the weakening of papal authority, this reading frames their actions as secondary to the theological imperative. They are beneficiaries of the institutional separation, not its primary cause.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers, beneficiary,
    institutional, generational, arbitrage, regional).

% Analyze the theological arguments and their historical impact, often affirming the internal coherence and transformative power of Luther's doctrine as a genuine intellectual and spiritual event.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Re-established a perceived correct understanding of salvation and Christian life, coordinating believers around a new theological paradigm based on scripture alone, resolving perceived doctrinal confusion and corruption.
% TRANSFER_FUNCTION: Transferred spiritual authority from the institutional hierarchy of the Roman Catholic Church to individual conscience and scripture, and transferred allegiance of believers from Rome to reformed churches.
% ABSENT_VOICES: The voices of those who sought reform within the existing Catholic structure, without advocating for schism, are often downplayed or framed as insufficient in this reading. They would argue for internal renewal rather than separation.
% DISAPPEARANCE_RATIONALE: If the theological breakthrough of justification by faith alone had not occurred or been accepted, the religious and political landscape of Europe would have remained fundamentally different, with the Catholic Church retaining its unchallenged spiritual authority and the subsequent development of Protestantism never taking place.
% FOUNDING_PROBLEM: The Roman Catholic Church was perceived to be in doctrinal error regarding salvation, particularly through the sale of indulgences and an overemphasis on works, leading to spiritual anxiety and corruption.
% FOUNDING_PROBLEM_CORROBORATION: Protestant theological traditions and many historical theologians attest that the doctrinal problem was real and required Luther's intervention. While the Catholic Church disputes the 'error' claim, the historical evidence of spiritual unrest and calls for reform from various sources corroborates the existence of a significant problem, even if its nature is contested.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The low extractiveness (0.15) and suppression (0.25) reflect the reading's view that the constraint emerged from a genuine theological necessity, not from a desire for extraction. The 'emerges_naturally: true' flag, despite beneficiaries, is intentional FSM authoring: this reading presents the theological truth as a natural law, even if it benefits certain groups. The low theater ratio (0.05) indicates that the theological arguments were seen as genuinely functional, not performative. Accessibility collapse is high (0.8) because, from this perspective, once the 'truth' was understood, alternatives (like remaining in the Catholic Church) became untenable for those who accepted it. Resistance is low (0.1) because the 'truth' itself is seen as compelling, with opposition framed as resistance to truth rather than to an extractive system.
 *
 * PERSPECTIVAL GAP:
 *   The Roman Catholic Church, from its own perspective, would experience this event as a profound institutional attack and schism, not a 'theological climb'. This reading's framing of the Church as a 'victim of theological correction' is precisely the point of divergence from other readings, which might frame it as a political target or an institution undergoing internal collapse.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant believers and reformed theology are direct beneficiaries (d near 0.0) as they gain spiritual clarity and institutional form. Martin Luther is the agenda-setter, driving the theological change. The Roman Catholic Church is the primary victim (d near 1.0) as it loses authority and adherents due to its perceived doctrinal errors. Secular rulers are secondary beneficiaries, gaining political leverage from the institutional separation, but their role is subservient to the theological driver in this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the theological innovation as mere extraction by emphasizing the genuine doctrinal content and its perceived necessity. The 'mandate' for separation is seen as arising directly from the 'rediscovery' of a fundamental truth, rather than from institutional inertia or rent-seeking. The constraint's persistence is tied to the enduring truth of the doctrine, not to a decaying function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_causation,
    'Was the Reformation primarily driven by theological innovation, or were political and economic factors the true underlying cause, with theology serving as a rationalization?',
    'Detailed counterfactual historical analysis: would the institutional separation have occurred without Luther''s specific theological claims, or would the political conditions have found another catalyst?',
    'If political factors were primary, this ''theological climb'' reading would be reclassified as a ''political_swap_reading'' or ''composite_overdetermination_reading'', with significantly higher extractiveness and suppression attributed to the political actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_causation, empirical, 'Ambiguity regarding the primary causal driver of the Reformation.').

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is justification by faith alone a ''natural law'' of Christian theology, or a constructed doctrine that gained ascendancy through historical contingency and institutional power?',
    'Comparative theological analysis across diverse Christian traditions and historical periods, assessing the universality and internal coherence of the doctrine independent of its historical emergence.',
    'If it is a constructed doctrine, the ''emerges_naturally: true'' flag would be false, and the constraint would be reclassified from Mountain to a more constructed type (e.g., Rope or Tangled Rope), reflecting its human-made nature and the beneficiaries it creates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether the theological claim is a natural law or a human construct.').

omega_variable(
    periodization_tightness,
    'Is the tight periodization (1517-1555) appropriate for capturing the ''theological climb'', or does it artificially truncate a longer, more complex process of doctrinal development and institutional change?',
    'Historical scholarship that examines the pre-Reformation calls for reform and post-Reformation theological developments, assessing the continuity and discontinuity of doctrinal evolution.',
    'A broader periodization might reveal a more gradual ''climb'' or integrate elements of ''institutional collapse'' and ''political swap'' that are downplayed in this reading, pushing it towards a ''composite_overdetermination_reading''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(periodization_tightness, conceptual, 'Whether the chosen timeframe accurately reflects the theological event.').


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
narrative_ontology:measurement(refo_tr_t1535, reformation_event_boundary__theological_climb_reading, theater_ratio, 1535, 0.05).
narrative_ontology:measurement(refo_tr_t1545, reformation_event_boundary__theological_climb_reading, theater_ratio, 1545, 0.05).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.05).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.1).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1525, 0.12).
narrative_ontology:measurement(refo_be_t1535, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1535, 0.14).
narrative_ontology:measurement(refo_be_t1545, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1545, 0.15).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.2).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.22).
narrative_ontology:measurement(refo_su_t1535, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1535, 0.24).
narrative_ontology:measurement(refo_su_t1545, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1545, 0.25).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
