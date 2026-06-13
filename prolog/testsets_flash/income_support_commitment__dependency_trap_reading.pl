% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes unconditional income support from the
 *   perspective that it acts as a work-disincentive, leading to skill atrophy
 *   and increased reliance on the state. It is a specific reading of the
 *   broader 'income_support_commitment' kernel, emphasizing the negative
 *   consequences for individual agency and societal productivity. The
 *   constraint is framed as a Tangled Rope because it offers a form of
 *   'coordination' (basic income security) but at the 'cost' of extracting
 *   productive capacity and fostering dependence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.65).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.7).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '839af6ac-35de-4aad-a86c-7263a68d1773').
narrative_ontology:cs_kernel_codification('839af6ac-35de-4aad-a86c-7263a68d1773', formalized).
narrative_ontology:cs_authority_grounding('839af6ac-35de-4aad-a86c-7263a68d1773', lineage).
narrative_ontology:cs_interpretation_layer_present('839af6ac-35de-4aad-a86c-7263a68d1773').
narrative_ontology:cs_reading_relation('839af6ac-35de-4aad-a86c-7263a68d1773', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('839af6ac-35de-4aad-a86c-7263a68d1773', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('839af6ac-35de-4aad-a86c-7263a68d1773', foundational, labor_is_primary_source_of_dignity_and_value).
narrative_ontology:cs_axiom_status(labor_is_primary_source_of_dignity_and_value, holdable).
narrative_ontology:cs_axiom_grounding('839af6ac-35de-4aad-a86c-7263a68d1773', labor_is_primary_source_of_dignity_and_value, deontological).
narrative_ontology:cs_axiom('839af6ac-35de-4aad-a86c-7263a68d1773', foundational, unconditional_support_erodes_human_capital).
narrative_ontology:cs_axiom_status(unconditional_support_erodes_human_capital, holdable).
narrative_ontology:cs_axiom_grounding('839af6ac-35de-4aad-a86c-7263a68d1773', unconditional_support_erodes_human_capital, empirically_contingent).
narrative_ontology:cs_reference_frame('839af6ac-35de-4aad-a86c-7263a68d1773', productive_citizenry_with_minimal_state_dependence).
narrative_ontology:cs_drift_state('839af6ac-35de-4aad-a86c-7263a68d1773', contemporary_welfare_state_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('839af6ac-35de-4aad-a86c-7263a68d1773', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, unconditional_income_recipients_exiting_labor).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, individuals_with_atrophied_skills).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the perceived transfer of resources from productive labor to non-productive consumption, and the 'cost' of lost human capital. Suppression (0.70) arises from the perceived reduction in individual agency and the difficulty of re-entering the labor market once skills have atrophied, effectively 'trapping' individuals in a state of dependence. The theater ratio is low (0.20) as the system is genuinely intended to provide support, but its negative side effects are seen as substantial. Accessibility collapse (0.40) is moderate, as individuals still technically have the option to seek work, but the disincentives and skill loss make it harder. Resistance (0.55) is moderate, coming from both taxpayers who resent funding non-participation and advocates for more conditional support.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of working taxpayers, this constraint is highly extractive, as they bear the costs without direct benefit. For individuals whose skills atrophy, it is also extractive, as their long-term economic prospects are diminished. For the recipients who choose to exit the labor market, it appears as a benefit, providing a 'floor' of security. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'unconditional_income_recipients_exiting_labor' are beneficiaries (d near 0.0) as they receive direct financial support and reduced pressure to work. 'Working_taxpayers' are victims (d near 1.0) as they fund the system without direct benefit and perceive a loss of societal productivity. 'Individuals_with_atrophied_skills' are also victims (d near 1.0) due to the long-term negative impact on their human capital. The 'state_bureaucracy' is an agenda-setter, administering the system and maintaining its structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the original mandate of income support (poverty alleviation) has been distorted, leading to unintended negative consequences that create a new, extractive dynamic. The constraint's persistence is due to the political difficulty of reforming or removing established welfare programs, even if their perceived effects are detrimental. The classification as Tangled Rope captures this hybrid nature: a genuine coordination function (basic security) intertwined with asymmetric extraction (dependency and skill loss).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine dependency trap, or an enabler of autonomy (freedom_floor_reading) or an inefficient allocation (targeting_efficiency_reading)?',
    'Longitudinal studies on labor market participation, skill development, and subjective well-being of recipients, disaggregated by demographic and economic context.',
    'If resolved as a dependency trap, it supports reclassification towards Snare or a highly extractive Tangled Rope. If resolved as a freedom floor, it supports reclassification towards Rope or Scaffold. If resolved as inefficient, it supports reclassification towards a less extractive Tangled Rope or Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'This constraint is the ''dependency_trap_reading'' of the ''income_support_commitment'' kernel. Sibling readings include ''freedom_floor_reading'' and ''targeting_efficiency_reading''.').

omega_variable(
    work_disincentive_causality,
    'To what extent does unconditional income support directly cause work disincentive and skill atrophy, versus merely reflecting pre-existing labor market conditions or individual preferences?',
    'Controlled trials comparing labor market outcomes and skill development in groups receiving unconditional income versus control groups, controlling for baseline socio-economic factors.',
    'Strong causal evidence for work disincentive and skill atrophy would increase the measured extractiveness and suppression, pushing the classification towards Snare. Weak or absent causality would reduce these metrics, pushing towards Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_disincentive_causality, empirical, 'Ambiguity regarding the causal link between unconditional income and negative labor market outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__dependency_trap_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__dependency_trap_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__dependency_trap_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(inco_su_t15, income_support_commitment__dependency_trap_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_commitment' kernel, focusing on the negative consequences of unconditional income support. Sibling readings include 'freedom_floor_reading' (emphasizing autonomy) and 'targeting_efficiency_reading' (emphasizing efficient allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
