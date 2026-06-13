% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Unified AI Risk Governance Framework (Bridge Reading)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents a 'bridge reading' of AI risk governance,
 *   asserting that both present harms (e.g., bias, misinformation) and
 *   existential risks (e.g., unaligned superintelligence) are interconnected
 *   and require unified governance frameworks. It seeks to overcome the false
 *   dichotomy often presented in the AI ethics and safety discourse. The
 *   constraint aims to coordinate research and policy efforts across these
 *   traditionally separate domains.
 *
 * KEY AGENTS:
 *   - bridging_institutions: Primary beneficiary (institutional/arbitrage) — facilitate cross-disciplinary work.
 *   - integrated_researchers: Beneficiary (moderate/mobile) — benefit from funding and legitimacy for interdisciplinary work.
 *   - present_marginalized_populations: Victim (powerless/trapped) — bear present harms, often overlooked by long-term focus.
 *   - future_humanity: Victim (powerless/generational) — bears existential risks, often overlooked by short-term focus.
 *   - existential_risk_advocates: Payer (organized/constrained) — must broaden their focus and share resources.
 *   - near_term_harms_advocates: Payer (organized/constrained) — must consider long-term implications and share resources.
 *   - policy_makers: Agenda setter (institutional/constrained) — responsible for implementing governance frameworks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.45).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.3).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Unified AI Risk Governance Framework (Bridge Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "ai_governance/technology_ethics/risk_assessment").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '3b7567b8-b3e8-4244-a523-255aec03177c').
narrative_ontology:cs_kernel_codification('3b7567b8-b3e8-4244-a523-255aec03177c', formalized).
narrative_ontology:cs_authority_grounding('3b7567b8-b3e8-4244-a523-255aec03177c', expertise).
narrative_ontology:cs_interpretation_layer_present('3b7567b8-b3e8-4244-a523-255aec03177c').
narrative_ontology:cs_reading_relation('3b7567b8-b3e8-4244-a523-255aec03177c', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b7567b8-b3e8-4244-a523-255aec03177c', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('3b7567b8-b3e8-4244-a523-255aec03177c', foundational, risk_spectrum_interconnected).
narrative_ontology:cs_axiom_status(risk_spectrum_interconnected, holdable).
narrative_ontology:cs_axiom_grounding('3b7567b8-b3e8-4244-a523-255aec03177c', risk_spectrum_interconnected, empirically_contingent).
narrative_ontology:cs_axiom('3b7567b8-b3e8-4244-a523-255aec03177c', foundational, unified_governance_optimal).
narrative_ontology:cs_axiom_status(unified_governance_optimal, holdable).
narrative_ontology:cs_axiom_grounding('3b7567b8-b3e8-4244-a523-255aec03177c', unified_governance_optimal, instrumental).
narrative_ontology:cs_reference_frame('3b7567b8-b3e8-4244-a523-255aec03177c', integrated_risk_management_paradigm).
narrative_ontology:cs_drift_state('3b7567b8-b3e8-4244-a523-255aec03177c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3b7567b8-b3e8-4244-a523-255aec03177c', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).
:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the cost of shifting established research agendas and institutional priorities, and the diffuse nature of the 'victims' (both present and future populations). Suppression (0.30) is relatively low, as this reading relies more on intellectual persuasion and institutional incentives than direct coercion. Theater ratio (0.20) is also low, as the effort to bridge these concerns is generally genuine, though some performative aspects may exist to secure funding. Accessibility collapse is moderate (0.40) as alternative, siloed approaches still exist but are less effective under this framework. Resistance (0.50) is moderate due to entrenched disciplinary boundaries and competing funding priorities.
 *
 * PERSPECTIVAL GAP:
 *   Bridging institutions and integrated researchers experience this as a beneficial coordination mechanism, enabling more holistic approaches. However, advocates for either purely existential or purely near-term risks may perceive it as an extractive demand to dilute their focus or share scarce resources, leading to a higher perceived extractiveness from their seats. Policy makers may see it as a necessary, albeit complex, coordination challenge.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions and integrated researchers are beneficiaries (d near 0.0) as they gain legitimacy and resources for their interdisciplinary work. Present marginalized populations and future humanity are victims (d near 1.0) as they are the ultimate targets of the risks this framework seeks to mitigate, bearing the costs of inaction or misdirection. Advocates for single-focus approaches are payers (d near 0.7) as they must cede some autonomy and resources to the unified framework. Policy makers are agenda setters (d near 0.5) as they administer the framework, balancing competing demands.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint aims to prevent mandatrophy by ensuring that the mandate of AI risk governance remains comprehensive, addressing both present and future concerns. Without this bridge, the mandate could atrophy into either a purely technical 'safety' problem (ignoring social harms) or a purely 'ethics' problem (ignoring catastrophic risks), leading to an incomplete and ultimately ineffective governance structure. The 'contested' status of the founding problem reflects the ongoing debate about whether the original problem (fragmented risk assessment) is truly being addressed or if the framework itself is becoming a new source of contention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''bridge'' between near-term and long-term AI risks, or a rhetorical framing to secure resources for one side?',
    'Empirical analysis of funding flows and research outputs: if resources disproportionately flow to one side despite the ''unified'' framing, reclassify as a disguised reading.',
    'If a genuine bridge, it facilitates comprehensive risk mitigation. If rhetorical, it functions as a Snare or Tangled Rope, extracting resources under false pretenses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''bridge_reading'' of the ''ai_risk_governance_priority'' kernel. Sibling readings (existential_risk_reading, near_term_harms_reading) would change the victim set and beneficiary structure, focusing on either future humanity or present marginalized populations exclusively. The disagreement is located in the scope and prioritization of AI risks.').

omega_variable(
    structural_fragility_of_bridging,
    'Can the ''bridging institutions'' effectively sustain the unified framework, or is it inherently fragile due to the deep disciplinary and incentive divides?',
    'Longitudinal study of cross-disciplinary collaboration metrics, funding stability for integrated research, and resilience to political pressure from single-focus advocacy groups.',
    'If fragile, the framework''s coordination function is weak, and it risks collapsing into one of the more extractive sibling readings. If robust, it genuinely reduces the overall extractiveness of fragmented governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_fragility_of_bridging, empirical, 'The unified framework depends on a handful of broker actors; its persistence is uncertain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(ai_r_tr_t30, ai_risk_governance_priority__bridge_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(ai_r_be_t30, ai_risk_governance_priority__bridge_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.29).
narrative_ontology:measurement(ai_r_su_t30, ai_risk_governance_priority__bridge_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_ethics_research_funding).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_safety_research_funding).

% DUAL FORMULATION NOTE:
% This constraint is the 'bridge_reading' of the 'ai_risk_governance_priority' kernel, which also includes 'existential_risk_reading' and 'near_term_harms_reading'. These three constraints represent different framings of the same underlying problem of AI risk prioritization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
