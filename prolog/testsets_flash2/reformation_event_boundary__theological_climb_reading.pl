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
 *   'reformation_event_boundary' kernel. It posits that the Reformation was
 *   primarily a theological innovation, driven by Luther's rediscovery of
 *   justification by faith alone, which constituted a genuine doctrinal
 *   breakthrough. This breakthrough necessitated institutional separation
 *   from the Roman Catholic Church, which is viewed as a victim of
 *   theological correction. The periodization is tight (1517-1555), focusing
 *   on the initial theological and institutional rupture. This reading frames
 *   the event as a 'climb' towards a more accurate understanding of
 *   scripture.
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
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '09604df1-c611-4efc-92cc-ce2df36307b7').
narrative_ontology:cs_kernel_codification('09604df1-c611-4efc-92cc-ce2df36307b7', fixed_text).
narrative_ontology:cs_authority_grounding('09604df1-c611-4efc-92cc-ce2df36307b7', lineage).
narrative_ontology:cs_interpretation_layer_present('09604df1-c611-4efc-92cc-ce2df36307b7').
narrative_ontology:cs_reading_relation('09604df1-c611-4efc-92cc-ce2df36307b7', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('09604df1-c611-4efc-92cc-ce2df36307b7', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('09604df1-c611-4efc-92cc-ce2df36307b7', foundational, justification_by_faith_alone_is_scriptural_truth).
narrative_ontology:cs_axiom_status(justification_by_faith_alone_is_scriptural_truth, holdable).
narrative_ontology:cs_axiom_grounding('09604df1-c611-4efc-92cc-ce2df36307b7', justification_by_faith_alone_is_scriptural_truth, theological).
narrative_ontology:cs_axiom('09604df1-c611-4efc-92cc-ce2df36307b7', foundational, papal_authority_is_not_supreme_in_doctrine).
narrative_ontology:cs_axiom_status(papal_authority_is_not_supreme_in_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('09604df1-c611-4efc-92cc-ce2df36307b7', papal_authority_is_not_supreme_in_doctrine, theological).
narrative_ontology:cs_reference_frame('09604df1-c611-4efc-92cc-ce2df36307b7', scriptural_primacy_and_individual_conscience).
narrative_ontology:cs_drift_state('09604df1-c611-4efc-92cc-ce2df36307b7', contemporary_ecumenical_dialogue, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('09604df1-c611-4efc-92cc-ce2df36307b7', '').
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

% Suffered a loss of authority, membership, and assets as a result of the theological challenge. From this reading's perspective, the Church was a victim of theological correction, forced to confront its doctrinal errors.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, roman_catholic_church, payer,
    institutional, civilizational, constrained, global).

% The primary agent of the theological breakthrough, whose insights into justification by faith alone initiated the movement. His commitment to this doctrine was absolute, making exit unthinkable.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, martin_luther, agenda_setter,
    powerful, biographical, identity_locked, regional).

% Benefited from the weakening of papal authority, gaining greater control over religious affairs and church lands within their territories. While not the primary theological drivers, they were instrumental in institutionalizing the separation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers, beneficiary,
    institutional, generational, arbitrage, regional).

% Analyze the theological arguments and their historical impact, often affirming the intellectual coherence and transformative power of Luther's insights as a genuine doctrinal innovation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a new theological framework for understanding salvation and Christian life, coordinating the beliefs and practices of a growing number of adherents around a rediscovered scriptural truth.
% TRANSFER_FUNCTION: Transferred spiritual authority from the institutional hierarchy of the Roman Catholic Church to the individual believer's conscience and direct relationship with scripture, leading to a redistribution of religious power and resources.
% ABSENT_VOICES: Those who prioritized institutional unity and political stability over theological purity, or those who saw the Reformation as primarily a power struggle, would object to this reading's emphasis on theological innovation as the sole driver. Their perspectives are often marginalized in this narrative.
% DISAPPEARANCE_RATIONALE: If the theological breakthrough of justification by faith alone were retroactively nullified, the entire trajectory of Western religious, political, and social history from the 16th century onward would be fundamentally altered. The rise of Protestantism, the wars of religion, the development of modern nation-states, and the Enlightenment would all be unrecognizable.
% FOUNDING_PROBLEM: The Roman Catholic Church's perceived corruption, sale of indulgences, and theological errors, particularly regarding the nature of salvation and the role of good works.
% FOUNDING_PROBLEM_CORROBORATION: Protestant theologians and historians attest that the founding problem was definitively solved by Luther's insights. Catholic historians and ecumenists, while acknowledging historical abuses, would contest the 'dead' status of the theological problem, arguing for continuity and reform within the existing tradition. Independent historical analysis from outside the benefiting parties supports that the specific abuses (e.g., indulgences) were addressed, but the broader theological disagreements remain contested.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.15) is low because the 'climb' is seen as a liberation from false doctrine, not a new form of extraction. Any 'cost' is framed as the necessary price of truth. Suppression (0.25) is also low, reflecting the idea that the new doctrine spread due to its inherent truth, not coercion, though some institutional resistance was met. Theater ratio (0.05) is minimal, as the theological claims are presented as genuine and direct. Accessibility collapse is high (0.8) because, from this perspective, once the 'truth' of justification by faith alone is understood, alternatives (like salvation through works or papal authority) collapse as viable options. Resistance (0.1) is low because the theological truth is seen as self-evident to those who embrace it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Protestant believers, this was a necessary and liberating theological correction. From the Roman Catholic Church's perspective, it was a schism driven by heresy and political opportunism. This story explicitly adopts the 'theological climb' perspective, framing the Church as a payer of theological costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant believers and reformed theology are the primary beneficiaries, experiencing liberation and vindication. The Roman Catholic Church is the victim, losing authority and adherents due to its theological errors. Martin Luther is the agenda-setter, driving the doctrinal change. Secular rulers are secondary beneficiaries, gaining political leverage. Historical theologians act as observers, often affirming the theological validity of the breakthrough.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_causation,
    'Was the Reformation primarily a theological innovation, or was theology a post-hoc rationalization for political and economic shifts?',
    'Detailed historical analysis of primary sources, focusing on the motivations of key actors and the sequence of events, particularly in regions where political outcomes diverged from theological alignment.',
    'If primarily political, this constraint would be reclassified as a Snare or Tangled Rope, with secular rulers as primary beneficiaries and theological claims as cover. If primarily theological, this Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_causation, empirical, 'Ambiguity regarding the primary causal driver of the Reformation: theological conviction versus political/economic opportunism.').

omega_variable(
    doctrinal_breakthrough_objectivity,
    'Is ''justification by faith alone'' an objective doctrinal breakthrough, or an interpretive choice among valid theological options?',
    'Comparative theological analysis across different Christian traditions and historical periods, assessing the coherence and scriptural grounding of alternative doctrines of salvation. This is a conceptual, not empirical, resolution.',
    'If an objective breakthrough, the ''emerges_naturally'' claim for this Mountain is strengthened. If an interpretive choice, the constraint''s naturalness is weakened, pushing it towards a constructed type (e.g., Rope or Tangled Rope) that coordinates a specific theological interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_breakthrough_objectivity, conceptual, 'The objectivity of Luther''s theological ''rediscovery'' versus its status as a specific interpretive tradition.').

omega_variable(
    periodization_tightness,
    'Is the tight periodization (1517-1555) appropriate for understanding the Reformation as a theological event, or does it obscure longer-term political, social, and economic factors?',
    'Comparative historical analysis using broader periodizations (e.g., 14th-17th centuries) to assess the relative weight of theological versus other factors across different temporal scales.',
    'A broader periodization might reveal the theological ''climb'' as one component within a larger, more complex, and potentially more extractive historical process, pushing this constraint towards a ''composite_overdetermination_reading'' or ''political_swap_reading'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_tightness, empirical, 'Whether the chosen periodization accurately captures the primary theological nature of the event or artificially isolates it from other causal factors.').


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
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.2).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.22).
narrative_ontology:measurement(refo_su_t1535, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1535, 0.23).
narrative_ontology:measurement(refo_su_t1545, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1545, 0.24).
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
