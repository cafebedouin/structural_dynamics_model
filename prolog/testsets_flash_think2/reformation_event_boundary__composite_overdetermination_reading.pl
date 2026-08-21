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
 *   This constraint represents the historiographical understanding that the
 *   Reformation was an overdetermined composite event, meaning it cannot be
 *   reduced to a single causal driver (theological, political, or
 *   institutional) but rather emerged from the simultaneous and irreducible
 *   interplay of multiple factors. This reading challenges simpler,
 *   monocausal narratives and emphasizes the inherent complexity of the
 *   historical phenomenon. It is one reading of the
 *   'reformation_event_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.05).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.08).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation Event Boundary: Composite Overdetermination Reading").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:emerges_naturally(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '48febf75-3e98-4542-8d08-343522f092aa').
narrative_ontology:cs_kernel_codification('48febf75-3e98-4542-8d08-343522f092aa', fixed_text).
narrative_ontology:cs_authority_grounding('48febf75-3e98-4542-8d08-343522f092aa', expertise).
narrative_ontology:cs_interpretation_layer_present('48febf75-3e98-4542-8d08-343522f092aa').
narrative_ontology:cs_reading_relation('48febf75-3e98-4542-8d08-343522f092aa', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('48febf75-3e98-4542-8d08-343522f092aa', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('48febf75-3e98-4542-8d08-343522f092aa', foundational, reformation_irreducibly_composite).
narrative_ontology:cs_axiom_status(reformation_irreducibly_composite, holdable).
narrative_ontology:cs_axiom_grounding('48febf75-3e98-4542-8d08-343522f092aa', reformation_irreducibly_composite, empirically_contingent).
narrative_ontology:cs_axiom('48febf75-3e98-4542-8d08-343522f092aa', foundational, no_single_causal_driver).
narrative_ontology:cs_axiom_status(no_single_causal_driver, holdable).
narrative_ontology:cs_axiom_grounding('48febf75-3e98-4542-8d08-343522f092aa', no_single_causal_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('48febf75-3e98-4542-8d08-343522f092aa', post_revisionist_historiography).
narrative_ontology:cs_drift_state('48febf75-3e98-4542-8d08-343522f092aa', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('48febf75-3e98-4542-8d08-343522f092aa', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, historical_complexity_thesis).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, multi_causal_historical_analysis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars who analyze the historical record of the Reformation, grappling with its multiple, interwoven causes and effects. They are committed to understanding the event in its full complexity, resisting reductionist narratives.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historiographers, observer,
    analytical, generational, analytical, global).

% Scholars focused on the theological dimensions of the Reformation, who, from this reading, acknowledge the interplay of doctrine with political and social forces without reducing the event solely to theology.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, theologians_of_reformation, observer,
    analytical, generational, analytical, global).

% Scholars focused on the political dimensions of the Reformation, who, from this reading, acknowledge the interplay of power dynamics with theological and social forces without reducing the event solely to politics.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, political_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates historical understanding by providing a framework that acknowledges the irreducible simultaneity and interdependence of theological, institutional, and political factors in the Reformation, preventing oversimplification.
% TRANSFER_FUNCTION: No direct transfer of material resources. It transfers a more nuanced and complex understanding of historical causality, requiring intellectual effort from those who adopt it.
% ABSENT_VOICES: Historians or ideologues committed to monocausal explanations (e.g., 'the Reformation was *only* about Luther's theology' or 'it was *just* a power grab') would object, as this reading directly challenges their reductionist narratives. They are absent from the analytical consensus that embraces overdetermination.
% DISAPPEARANCE_RATIONALE: The historical events of the Reformation would remain unchanged. Only the historiographical consensus on how to interpret and periodize them would shift, likely reverting to simpler, less nuanced causal models.
% FOUNDING_PROBLEM: The problem of adequately explaining a complex historical event like the Reformation, which resists simple causal attribution or linear periodization due to its simultaneous theological, institutional, and political dimensions.
% FOUNDING_PROBLEM_CORROBORATION: Independent historical analysis, drawing on diverse primary sources and interdisciplinary methods, consistently reveals the multi-layered and overdetermined nature of the Reformation, corroborating this reading from outside any single disciplinary or ideological beneficiary set.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_unchanged).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Mountain because the overdetermined, composite nature of the Reformation is presented as an inherent feature of the historical phenomenon itself, an epistemological limit that emerges naturally from the evidence. It is not a human construct designed to extract or coordinate, but a descriptive claim about the nature of the event. Extractiveness, suppression, and theater ratio are low because the constraint itself (the recognition of complexity) does not directly extract, coerce, or perform. Accessibility collapse is high because once the overdetermined nature is understood, simpler, monocausal explanations become intellectually untenable. Resistance is low among serious scholars, as the evidence for complexity is robust.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap in how this specific reading (composite overdetermination) is experienced by those who adopt it. The 'gap' exists between this reading and alternative, simpler readings, which this constraint implicitly critiques. The engine's classification will reflect the inherent, mountain-like nature of this epistemological claim.
 *
 * DIRECTIONALITY LOGIC:
 *   As an epistemological constraint describing an inherent historical complexity, there are no direct beneficiaries or victims in the sense of agents gaining or losing material resources. All listed stakeholders are 'observers' or 'analytical' seats, engaging with the constraint intellectually. Their 'directionality' is neutral or slightly beneficial in that it provides a more accurate and robust framework for historical inquiry.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historiographical_framing_ambiguity,
    'Is the ''overdetermined composite'' nature of the Reformation an inherent feature of the historical event, or a product of specific historiographical framing choices?',
    'Comparative analysis of historical methodologies across different eras and cultures: if the composite nature is consistently identified regardless of methodological school, it supports inherent status; if it varies with theoretical lens, it suggests framing dependence.',
    'If framing-dependent, the constraint''s ''emerges_naturally'' claim would be weakened, potentially shifting its classification from Mountain towards a Rope (a coordinated way of understanding) or even a conceptual Snare (if the framing serves to obscure simpler causal narratives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historiographical_framing_ambiguity, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of historical overdetermination.').

omega_variable(
    periodization_scheme_contestability,
    'Given the composite nature, is any single periodization scheme for the Reformation inherently arbitrary or does the composite reading allow for multiple valid, context-dependent periodizations?',
    'Analysis of the utility and explanatory power of different periodization schemes when applied to specific sub-events or regional contexts within the broader Reformation.',
    'If all periodizations are arbitrary, it reinforces the ''mountain'' aspect of irreducible complexity. If multiple valid periodizations emerge, it suggests a ''rope'' function for coordinating different analytical foci.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_scheme_contestability, conceptual, 'Contestability of periodization schemes within a composite historical event.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1950, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(refo_tr_t1970, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(refo_tr_t1990, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(refo_tr_t2010, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(refo_tr_t2024, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(refo_be_t1950, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(refo_be_t1970, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(refo_be_t1990, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(refo_be_t2010, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(refo_be_t2024, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1950, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1950, 0.08).
narrative_ontology:measurement(refo_su_t1970, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement(refo_su_t1990, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1990, 0.08).
narrative_ontology:measurement(refo_su_t2010, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2010, 0.08).
narrative_ontology:measurement(refo_su_t2024, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2024, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, information_standard).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__political_swap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'reformation_event_boundary' kernel. Each reading offers a distinct interpretation of the Reformation's primary nature, leading to different structural properties and classifications. This 'composite_overdetermination_reading' emphasizes the irreducible complexity, while others focus on specific causal drivers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
