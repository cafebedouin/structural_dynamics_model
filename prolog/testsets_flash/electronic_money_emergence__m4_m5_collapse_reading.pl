% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: Electronic Money Emergence (M4/M5 Collapse Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint describes the 'emergence' of electronic money as a
 *   measurement artifact, specifically through the lens of the M4/M5
 *   statistical distinction used by central banks. Rather than reflecting a
 *   genuine historical or technological discontinuity, this reading argues
 *   that the distinction itself retroactively created the category, making
 *   the 'emergence' a classificatory piton. The constraint is claimed as a
 *   Piton because its primary function (accurate historical/theoretical
 *   description) has atrophied, but it persists due to institutional inertia
 *   and the performative maintenance of statistical consistency by central
 *   banks. It extracts from historians and theorists by distorting their
 *   analytical frameworks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.15).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.25).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "Electronic Money Emergence (M4/M5 Collapse Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '5498f01c-944d-45fd-b4e8-d862a50503ed').
narrative_ontology:cs_kernel_codification('5498f01c-944d-45fd-b4e8-d862a50503ed', formalized).
narrative_ontology:cs_authority_grounding('5498f01c-944d-45fd-b4e8-d862a50503ed', lineage).
narrative_ontology:cs_interpretation_layer_present('5498f01c-944d-45fd-b4e8-d862a50503ed').
narrative_ontology:cs_reading_relation('5498f01c-944d-45fd-b4e8-d862a50503ed', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('5498f01c-944d-45fd-b4e8-d862a50503ed', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('5498f01c-944d-45fd-b4e8-d862a50503ed', foundational, monetary_categories_are_statistical_constructs).
narrative_ontology:cs_axiom_status(monetary_categories_are_statistical_constructs, holdable).
narrative_ontology:cs_axiom_grounding('5498f01c-944d-45fd-b4e8-d862a50503ed', monetary_categories_are_statistical_constructs, conventional).
narrative_ontology:cs_axiom('5498f01c-944d-45fd-b4e8-d862a50503ed', foundational, emergence_is_a_measurement_artifact).
narrative_ontology:cs_axiom_status(emergence_is_a_measurement_artifact, holdable).
narrative_ontology:cs_axiom_grounding('5498f01c-944d-45fd-b4e8-d862a50503ed', emergence_is_a_measurement_artifact, empirically_contingent).
narrative_ontology:cs_reference_frame('5498f01c-944d-45fd-b4e8-d862a50503ed', statistical_measurement_consistency).
narrative_ontology:cs_drift_state('5498f01c-944d-45fd-b4e8-d862a50503ed', contemporary_monetary_theory, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('5498f01c-944d-45fd-b4e8-d862a50503ed', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, economic_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the statistical categories (M4/M5) that retroactively define 'electronic money' as a distinct category, even if the underlying monetary phenomena predate the distinction. They benefit from the stability of their measurement framework.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_banks, agenda_setter,
    institutional, generational, constrained, national).

% Are forced to reconcile historical monetary developments with a statistical classification that imposes a retroactive 'emergence' date, obscuring the continuous evolution of dematerialized money. Their work is made more complex by the artifact.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians, payer,
    moderate, generational, constrained, global).

% Struggle to develop coherent theories of money's evolution when the 'emergence' of electronic money is treated as a discrete event driven by statistical reclassification rather than underlying economic or technological shifts. The artifact distorts their models.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, economic_theorists, payer,
    moderate, generational, constrained, global).

% Develop new forms of digital value transfer that often predate or fall outside the M4/M5 classifications, yet their innovations are retroactively framed by these statistical distinctions. They would argue for a more fluid, practice-based definition of monetary innovation.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, financial_innovators, excluded,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, if arbitrary, statistical framework for central banks to measure and report on monetary aggregates, ensuring consistency in economic data reporting.
% TRANSFER_FUNCTION: Transfers conceptual clarity (for central banks) at the cost of historical and theoretical accuracy (for historians and theorists), by imposing a measurement artifact as a real emergence event.
% ABSENT_VOICES: Monetary historians and economic theorists who emphasize the continuous evolution of money and the artificiality of statistical boundaries are present but marginalized; financial innovators who operate outside these classifications are largely unheard in the definitional debate.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction vanished, central banks would adopt new statistical measures, but the underlying monetary phenomena (dematerialized value transfer) would continue unchanged. The 'emergence' as a measurement artifact would cease to be a point of confusion, but the actual historical development of electronic money would remain as it was.
% FOUNDING_PROBLEM: The need for consistent, quantifiable metrics to track the supply of money in an increasingly complex financial system, particularly as new forms of dematerialized value transfer emerged.
% FOUNDING_PROBLEM_CORROBORATION: Central banks continue to attest to the need for stable monetary aggregates for policy purposes. However, monetary historians and economic theorists, from outside the central banking community, corroborate that the *specific* M4/M5 distinction has created more confusion than clarity regarding the 'emergence' of electronic money, suggesting the problem is live but the solution is flawed.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_unchanged).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).
:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the 'extraction' is primarily conceptual and analytical distortion, not direct financial transfer. Suppression is also low (0.25) as there's no active coercion against alternative historical narratives, but the institutional weight of central bank statistics makes it difficult to ignore. Theater ratio is high (0.70) because the maintenance of the M4/M5 distinction is largely performative, serving to stabilize a measurement convention rather than accurately reflecting underlying monetary physics or historical evolution. Accessibility collapse is high (0.80) because once the statistical framework is adopted, it becomes difficult to conceptualize 'electronic money' outside its boundaries. Resistance is low (0.10) as the primary 'victims' are academics whose critiques have limited impact on central bank policy.
 *
 * PERSPECTIVAL GAP:
 *   Central banks perceive the M4/M5 distinction as a necessary coordination mechanism for monetary policy and reporting. Historians and theorists, however, experience it as an arbitrary imposition that distorts the historical record and theoretical understanding of money's evolution. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks, as agenda-setters, benefit from the stability and consistency of their statistical framework (d near beneficiary end). Monetary historians and economic theorists are the victims, bearing the cost of conceptual distortion and analytical complexity (d near target end). Financial innovators are excluded, as their innovations are often retroactively categorized by this framework, rather than shaping it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statistical_artifact_vs_real_event,
    'Is the ''emergence'' of electronic money a genuine historical/technological event, or primarily a statistical artifact of classification systems like M4/M5?',
    'Comparative historical analysis of dematerialized value transfer systems across different eras and cultures, independent of modern central bank classifications.',
    'If a genuine event, the constraint''s extractiveness and theater ratio would be lower, as the classification would reflect a real underlying phenomenon. If primarily an artifact, the Piton classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statistical_artifact_vs_real_event, conceptual, 'Ambiguity between a measurement artifact and a real historical event.').

omega_variable(
    central_bank_mandate_drift,
    'To what extent does the central bank''s mandate to maintain statistical consistency override its mandate to accurately reflect monetary reality?',
    'Analysis of central bank internal documents and policy debates regarding classification changes, particularly where they conflict with academic consensus on monetary history.',
    'If consistency consistently overrides accuracy, it reinforces the Piton classification and suggests a form of institutional capture by internal measurement conventions. If accuracy is prioritized, the constraint might be reclassified as a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_bank_mandate_drift, empirical, 'Drift in central bank mandate between statistical consistency and monetary accuracy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1980, 0.5).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1990, 0.6).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2000, 0.65).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2010, 0.68).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2020, 0.7).

% Extraction over time
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2000, 0.24).
narrative_ontology:measurement(elec_su_t2010, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(elec_su_t2020, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2020, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
