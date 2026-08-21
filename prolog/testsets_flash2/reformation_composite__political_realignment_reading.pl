% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Reformation as Political Realignment: Assertion of State Sovereignty
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint models the Reformation as a political event, where
 *   emerging nation-states and territorial rulers leveraged religious
 *   differentiation (e.g., Protestantism vs. Catholicism) to assert
 *   sovereignty against the universalist claims of the Holy Roman Empire and
 *   Papal authority. The principle of 'Cuius regio, eius religio' (whose
 *   realm, his religion) is the primary observable, demonstrating the direct
 *   link between religious affiliation and political control. This reading
 *   emphasizes the instrumental use of religious change for state-building
 *   and power consolidation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.78).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.85).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Reformation as Political Realignment: Assertion of State Sovereignty").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, 'd79ce9f1-112e-452f-8987-8e070ae75e6a').
narrative_ontology:cs_kernel_codification('d79ce9f1-112e-452f-8987-8e070ae75e6a', formalized).
narrative_ontology:cs_authority_grounding('d79ce9f1-112e-452f-8987-8e070ae75e6a', extraction).
narrative_ontology:cs_interpretation_layer_present('d79ce9f1-112e-452f-8987-8e070ae75e6a').
narrative_ontology:cs_reading_relation('d79ce9f1-112e-452f-8987-8e070ae75e6a', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d79ce9f1-112e-452f-8987-8e070ae75e6a', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('d79ce9f1-112e-452f-8987-8e070ae75e6a', foundational, religious_uniformity_as_state_prerogative).
narrative_ontology:cs_axiom_status(religious_uniformity_as_state_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('d79ce9f1-112e-452f-8987-8e070ae75e6a', religious_uniformity_as_state_prerogative, conventional).
narrative_ontology:cs_axiom('d79ce9f1-112e-452f-8987-8e070ae75e6a', foundational, sovereignty_requires_internal_religious_control).
narrative_ontology:cs_axiom_status(sovereignty_requires_internal_religious_control, holdable).
narrative_ontology:cs_axiom_grounding('d79ce9f1-112e-452f-8987-8e070ae75e6a', sovereignty_requires_internal_religious_control, instrumental).
narrative_ontology:cs_reference_frame('d79ce9f1-112e-452f-8987-8e070ae75e6a', medieval_overlapping_jurisdictions).
narrative_ontology:cs_drift_state('d79ce9f1-112e-452f-8987-8e070ae75e6a', peace_of_augsburg_1555, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d79ce9f1-112e-452f-8987-8e070ae75e6a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_nation_states).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_empire).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, local_religious_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserted the right to determine the religion of their territories ('Cuius regio, eius religio'), consolidating power and resources previously claimed by the Church or Empire. Benefited from seizing church lands and appointing their own clergy, reducing external interference.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    institutional, generational, constrained, regional).

% Used religious differentiation as a tool to define national identity and consolidate sovereignty against universalist claims of the Holy Roman Empire and the Papacy. Gained greater control over internal affairs and resources.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_nation_states, beneficiary,
    institutional, generational, constrained, national).

% Suffered a significant loss of political authority, territorial control, and tax revenue as its constituent states asserted religious independence. Its universalist claims were directly challenged and eroded.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_empire, payer,
    institutional, civilizational, trapped, continental).

% Lost spiritual and temporal jurisdiction over vast regions, along with substantial tithes and landholdings. Its claim to universal religious and political supremacy was fundamentally undermined, leading to a fragmentation of Christendom.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, civilizational, trapped, global).

% Were often forced to convert, emigrate, or face persecution if their personal faith did not align with that of their territorial ruler. Their religious freedom was directly suppressed by the principle of 'Cuius regio, eius religio'.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, local_religious_minorities, payer,
    powerless, biographical, trapped, local).

% While often initiating the theological debates, their ideas were rapidly co-opted and instrumentalized by political powers. Their primary concern was religious truth, but their impact was largely mediated and shaped by political agendas.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, theologians_and_reformers, observer,
    moderate, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for territorial rulers to coordinate their assertion of sovereignty against external imperial and papal claims, by aligning religious identity with political jurisdiction.
% TRANSFER_FUNCTION: Transferred political authority, land, and tax revenue from the Holy Roman Empire and Papacy to emerging nation-states and territorial rulers, using religious differentiation as the legitimizing mechanism.
% ABSENT_VOICES: Religious minorities who were forced to conform or flee, and those who sought a purely theological reformation without political instrumentalization, were largely silenced or marginalized by the 'Cuius regio, eius religio' principle.
% DISAPPEARANCE_RATIONALE: If the political realignment aspect of the Reformation vanished, the modern nation-state system as we know it would be fundamentally different, with potentially stronger imperial or papal authority persisting into later centuries. The separation of church and state, and the concept of national sovereignty, would have developed along different trajectories.
% FOUNDING_PROBLEM: The problem of overlapping jurisdictions and competing claims to authority between secular rulers, the Holy Roman Empire, and the Papacy, which hindered the consolidation of centralized state power.
% FOUNDING_PROBLEM_CORROBORATION: Historians of political economy and state formation corroborate that the problem of consolidating state power against external claims was a central driver for territorial rulers, and that religious differentiation provided a powerful, if often cynical, means to achieve this. This perspective is widely accepted in secular historical analysis.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because significant resources (church lands, tithes) and authority were transferred from imperial/papal entities to secular rulers. Suppression is also high, as the enforcement of 'Cuius regio, eius religio' directly suppressed religious dissent and minority faiths within a given territory. Theater ratio is low, as the political function was direct and effective, with religious arguments serving as legitimizing cover rather than primary drivers of the constraint's persistence. The metrics reflect the increasing consolidation of state power and suppression of alternatives over the period.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of territorial rulers, this was a necessary assertion of legitimate authority and a solution to political fragmentation. From the perspective of the Empire and Papacy, it was an illegitimate rebellion and a catastrophic loss of divinely ordained order. Religious minorities experienced it as direct coercion. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers and emerging nation-states are clear beneficiaries, gaining power and resources. The Holy Roman Empire and Papal authority are victims, suffering significant losses of jurisdiction and revenue. Local religious minorities are also victims, as their religious freedom was directly curtailed. Theologians, while initiating the intellectual ferment, are largely observers in this political reading, their ideas instrumentalized by more powerful actors.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'To what extent was the political realignment a consequence of genuine theological conviction among rulers, versus a cynical instrumentalization of religious dissent for secular gain?',
    'Detailed historical analysis of individual rulers'' motivations, private correspondence, and consistency of religious policy across changing political circumstances. Examination of cases where political and religious interests diverged.',
    'If primarily cynical, the extractiveness and suppression metrics are more accurately attributed to pure power-seeking. If genuine conviction played a significant role, the constraint might have a stronger ''rope'' component for the rulers, as they genuinely coordinated around a shared belief system, even if it victimized others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, conceptual, 'Ambiguity regarding the true drivers of political actors'' religious choices during the Reformation.').

omega_variable(
    causal_direction_of_cuius_regio,
    'Did the principle of ''Cuius regio, eius religio'' primarily enable state sovereignty, or was it a consequence of already emerging state power seeking a legitimizing framework?',
    'Comparative historical analysis of state formation processes in regions with and without strong Reformation influence, and detailed examination of the timing of political centralization relative to religious shifts.',
    'If it primarily enabled sovereignty, the constraint''s coordination function for rulers is stronger. If it was a consequence, the constraint''s extractive nature (from imperial/papal authority) is more pronounced, as it merely codified an existing power shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_direction_of_cuius_regio, empirical, 'Whether ''Cuius regio, eius religio'' was a cause or effect of state power consolidation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of religious minorities primarily structural (legal/political enforcement) or internalized (social pressure, fear of reprisal)?',
    'Analysis of individual testimonies, records of resistance, and the long-term persistence of minority faiths in clandestine forms. If suppression persisted after direct enforcement eased, it suggests internalized mechanisms.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as individuals carried the suppression with them. If purely structural, removal of the political enforcement would have led to immediate religious pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for religious minorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(refo_tr_t1540, reformation_composite__political_realignment_reading, theater_ratio, 1540, 0.15).
narrative_ontology:measurement(refo_tr_t1570, reformation_composite__political_realignment_reading, theater_ratio, 1570, 0.12).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__political_realignment_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.1).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.5).
narrative_ontology:measurement(refo_be_t1540, reformation_composite__political_realignment_reading, base_extractiveness, 1540, 0.65).
narrative_ontology:measurement(refo_be_t1570, reformation_composite__political_realignment_reading, base_extractiveness, 1570, 0.72).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__political_realignment_reading, base_extractiveness, 1600, 0.75).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.6).
narrative_ontology:measurement(refo_su_t1540, reformation_composite__political_realignment_reading, suppression_requirement, 1540, 0.7).
narrative_ontology:measurement(refo_su_t1570, reformation_composite__political_realignment_reading, suppression_requirement, 1570, 0.78).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__political_realignment_reading, suppression_requirement, 1600, 0.82).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, westphalian_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reformation_composite' kernel, focusing on the political instrumentalization of religious change. It influences and is influenced by the theological and technological readings, as all three aspects were intertwined in the historical event. It also directly affects the emergence of Westphalian sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
