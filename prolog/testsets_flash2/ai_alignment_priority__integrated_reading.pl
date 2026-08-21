% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated AI Alignment: Catastrophic and Present Harms as Complementary Priorities
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'integrated reading' of AI alignment
 *   priorities, which posits that addressing catastrophic (existential) risks
 *   and present-day (near-term) harms are complementary, not competing,
 *   objectives. It advocates for a dual-track approach in AI development and
 *   governance. This reading is one of three competing interpretations of the
 *   'AI alignment priority' kernel. The metrics reflect a moderate level of
 *   extraction and suppression, as resources are reallocated and some
 *   single-focus approaches are de-emphasized, but the overall goal is
 *   coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.45).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.3).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated AI Alignment: Catastrophic and Present Harms as Complementary Priorities").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, 'ee955c4e-61ea-4c94-93b9-cdc9c6235685').
narrative_ontology:cs_kernel_codification('ee955c4e-61ea-4c94-93b9-cdc9c6235685', formalized).
narrative_ontology:cs_authority_grounding('ee955c4e-61ea-4c94-93b9-cdc9c6235685', expertise).
narrative_ontology:cs_interpretation_layer_present('ee955c4e-61ea-4c94-93b9-cdc9c6235685').
narrative_ontology:cs_reading_relation('ee955c4e-61ea-4c94-93b9-cdc9c6235685', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee955c4e-61ea-4c94-93b9-cdc9c6235685', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('ee955c4e-61ea-4c94-93b9-cdc9c6235685', foundational, ai_risks_are_interdependent).
narrative_ontology:cs_axiom_status(ai_risks_are_interdependent, holdable).
narrative_ontology:cs_axiom_grounding('ee955c4e-61ea-4c94-93b9-cdc9c6235685', ai_risks_are_interdependent, empirically_contingent).
narrative_ontology:cs_axiom('ee955c4e-61ea-4c94-93b9-cdc9c6235685', foundational, ethical_governance_requires_holistic_risk_mitigation).
narrative_ontology:cs_axiom_status(ethical_governance_requires_holistic_risk_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('ee955c4e-61ea-4c94-93b9-cdc9c6235685', ethical_governance_requires_holistic_risk_mitigation, deontological).
narrative_ontology:cs_reference_frame('ee955c4e-61ea-4c94-93b9-cdc9c6235685', holistic_risk_management_paradigm).
narrative_ontology:cs_drift_state('ee955c4e-61ea-4c94-93b9-cdc9c6235685', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ee955c4e-61ea-4c94-93b9-cdc9c6235685', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, ai_developers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_generations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, marginalized_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, resource_constrained_ai_labs).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, single_focus_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing alignment strategies that balance both long-term catastrophic risk mitigation and short-term harm prevention. They bear the cost of dual-track research and development but benefit from broader societal legitimacy and reduced regulatory friction.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_developers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the mitigation of existential risks posed by advanced AI, ensuring a viable future. Their interests are represented by advocates in the present.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Benefit from the prevention of present-day harms such as algorithmic bias, discrimination, and surveillance, which disproportionately affect them. Their advocacy pushes for accountability and justice in AI deployment.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, marginalized_communities, beneficiary,
    organized, generational, constrained, global).

% Advocate for and design regulatory frameworks that integrate both catastrophic and present harm considerations. They shape the discourse and institutionalize the dual-priority approach.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_ethicists_and_policy_makers, agenda_setter,
    institutional, generational, analytical, global).

% Struggle to implement comprehensive dual-track alignment strategies due to limited funding and expertise, potentially slowing their innovation or market entry. They bear the cost of increased complexity and regulatory burden.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, resource_constrained_ai_labs, payer,
    moderate, immediate, constrained, regional).

% Researchers who previously focused exclusively on either existential risk or near-term harms find their work de-prioritized or require re-framing to fit the integrated approach, incurring professional costs and requiring skill adaptation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, single_focus_researchers, payer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI development and governance efforts by establishing a unified framework that addresses both long-term, high-impact, low-probability catastrophic risks and immediate, high-probability, localized harms, preventing a zero-sum competition for resources and attention between these critical areas.
% TRANSFER_FUNCTION: Transfers resources (funding, research attention, regulatory focus) from a purely singular-priority approach (either existential or near-term) to a balanced, integrated approach, ensuring both sets of risks receive adequate attention and mitigation efforts.
% ABSENT_VOICES: Extremist factions on both sides (those who dismiss near-term harms as trivial compared to existential risk, and those who dismiss existential risk as speculative compared to present harms) are marginalized by this integrated approach. They would argue for a return to a singular focus, but their views are not given equal weight in the integrated discourse.
% DISAPPEARANCE_RATIONALE: If this integrated approach vanished, the AI safety and ethics communities would likely revert to a polarized debate, with resources and attention disproportionately flowing to one priority over the other, leading to unaddressed risks and a fragmented governance landscape. AI development would proceed without a coherent ethical framework.
% FOUNDING_PROBLEM: The AI safety and ethics fields were becoming increasingly polarized, with advocates for existential risk and near-term harms competing for resources and framing the issues as mutually exclusive, leading to an incomplete and ineffective approach to AI governance.
% FOUNDING_PROBLEM_CORROBORATION: Leading AI ethics organizations, intergovernmental bodies, and independent academic researchers corroborate that the polarization was a significant impediment to effective AI governance, and that an integrated approach is essential for comprehensive risk mitigation. This is attested by joint policy papers and multi-stakeholder initiatives.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).
:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs imposed on those who must adapt to a dual-priority framework (e.g., resource-constrained labs, single-focus researchers) and the inherent overhead of coordinating complex, multi-faceted risk mitigation. Suppression (0.3) is also moderate, as it requires active effort to prevent the re-emergence of a singular-priority focus, but does not involve overt coercion. Theater ratio is low (0.1) as the integrated approach is genuinely functional, not performative. Accessibility collapse is moderate (0.4) because while alternatives (singular focus) are discouraged, they are not entirely eliminated. Resistance (0.5) is present from those who prefer a simpler, single-priority approach.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the integrated approach's proponents, this constraint is a necessary 'rope' for effective AI governance. However, from the perspective of those who advocate for a singular focus (either existential risk or near-term harms), it might be perceived as a 'tangled rope' or even a 'snare' due to the perceived dilution of their primary concern or the reallocation of resources away from their preferred area. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers and policy makers act as agenda-setters, bearing the costs of implementation but benefiting from a more robust and legitimate framework. Future generations and marginalized communities are primary beneficiaries, as their interests are explicitly protected. Resource-constrained labs and single-focus researchers are payers, experiencing increased demands and a shift in research priorities. The integrated approach aims for a net positive coordination outcome, but with unavoidable costs for certain actors.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by acknowledging that while there are costs and reallocations (extraction), the primary function is genuine coordination across previously competing priorities. It's not a pure extraction (snare) because it aims to benefit a broader set of stakeholders and solve a collective action problem (fragmented risk mitigation). It's not a pure coordination (rope) because it requires active enforcement and imposes costs on some actors, making it a hybrid. The 'mandate' is to integrate, and it is actively pursued, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_balance,
    'Is the actual allocation of resources (funding, research talent) truly balanced between catastrophic and present harms, or does one still implicitly dominate?',
    'Quantitative analysis of funding streams, research publications, and policy initiatives over time, disaggregated by risk type.',
    'If one priority consistently dominates, the ''integrated'' claim may be performative, pushing the constraint towards a higher theater_ratio or even a ''tangled_rope'' classification for the under-resourced priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_balance, empirical, 'Assesses whether resource allocation matches the stated integrated priority.').

omega_variable(
    framing_legitimacy_contest,
    'Does the ''integrated'' framing genuinely resolve the underlying philosophical and ethical tensions between different risk priorities, or does it merely paper over them for political expediency?',
    'Qualitative analysis of expert discourse, policy debates, and public reception; examination of whether fundamental disagreements persist despite the integrated language.',
    'If tensions remain unresolved, the constraint''s stability is lower, and it may be more susceptible to ''piton'' drift if the integrated mandate loses genuine buy-in, or ''tangled_rope'' if it becomes a tool for one side to subtly dominate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_legitimacy_contest, conceptual, 'Examines the depth of resolution achieved by the integrated framing.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''integrated_reading'' of AI alignment priorities, or does it subtly favor one priority over the other, making it a ''tangled_rope'' or ''snare'' for the disfavored priority?',
    'Comparison of this reading''s outcomes (resource allocation, policy impact, stakeholder satisfaction) against the ideal-type definitions of ''existential_risk_reading'' and ''nearterm_harms_reading''.',
    'If the reading is found to subtly favor one priority, its classification would shift towards ''tangled_rope'' or ''snare'' for the disadvantaged group, and its ''rope'' claim would be invalidated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''ai_alignment_priority'' kernel. Sibling readings include ''existential_risk_reading'' and ''nearterm_harms_reading''. This omega addresses whether this reading truly achieves integration or if it''s a disguised form of one of the singular priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__integrated_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__integrated_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__integrated_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__integrated_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__integrated_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__integrated_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__integrated_reading, suppression_requirement, 5, 0.23).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__integrated_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__integrated_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_governance_frameworks).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_safety_research_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
