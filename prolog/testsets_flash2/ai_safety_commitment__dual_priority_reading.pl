% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety: Dual Priority (Existential & Near-Term Harms)
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'dual priority' reading of the AI safety
 *   commitment, which posits that both existential (long-term, catastrophic)
 *   and near-term (present-day, societal) AI harms must be addressed as
 *   non-competing priorities. This reading attempts to unify the AI safety
 *   field, which has historically been divided between these two concerns.
 *   The challenge for this reading is maintaining coherence and effective
 *   resource allocation across such a broad mandate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.4).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.3).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety: Dual Priority (Existential & Near-Term Harms)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "ai_safety/technology_governance/risk_assessment").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'b33af631-6c7b-4498-b16c-c36bcb68aa47').
narrative_ontology:cs_kernel_codification('b33af631-6c7b-4498-b16c-c36bcb68aa47', formalized).
narrative_ontology:cs_authority_grounding('b33af631-6c7b-4498-b16c-c36bcb68aa47', expertise).
narrative_ontology:cs_interpretation_layer_present('b33af631-6c7b-4498-b16c-c36bcb68aa47').
narrative_ontology:cs_reading_relation('b33af631-6c7b-4498-b16c-c36bcb68aa47', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b33af631-6c7b-4498-b16c-c36bcb68aa47', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('b33af631-6c7b-4498-b16c-c36bcb68aa47', foundational, all_ai_risks_are_interconnected).
narrative_ontology:cs_axiom_status(all_ai_risks_are_interconnected, holdable).
narrative_ontology:cs_axiom_grounding('b33af631-6c7b-4498-b16c-c36bcb68aa47', all_ai_risks_are_interconnected, empirically_contingent).
narrative_ontology:cs_axiom('b33af631-6c7b-4498-b16c-c36bcb68aa47', foundational, comprehensive_approach_is_optimal).
narrative_ontology:cs_axiom_status(comprehensive_approach_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('b33af631-6c7b-4498-b16c-c36bcb68aa47', comprehensive_approach_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('b33af631-6c7b-4498-b16c-c36bcb68aa47', unified_risk_landscape).
narrative_ontology:cs_drift_state('b33af631-6c7b-4498-b16c-c36bcb68aa47', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b33af631-6c7b-4498-b16c-c36bcb68aa47', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_researchers_dual_priority).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, policy_makers_dual_priority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, public_advocacy_groups_dual_priority).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, ai_developers_dual_priority).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, comprehensive_risk_management).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, ethical_ai_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and conducts research into both existential and near-term AI risks, seeking to integrate these concerns into a unified safety agenda. Faces challenges in securing funding and public attention for a broad, complex mandate.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_researchers_dual_priority, agenda_setter,
    organized, generational, constrained, global).

% Seeks to develop regulatory frameworks that address the full spectrum of AI risks, from catastrophic long-term scenarios to immediate societal harms. Benefits from a more holistic approach but struggles with resource allocation and political feasibility.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_makers_dual_priority, beneficiary,
    institutional, biographical, constrained, national).

% Expected to implement safety measures for both near-term deployment and long-term alignment. Bears the cost of integrating diverse safety requirements, which can be perceived as competing for resources and attention within development cycles.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_developers_dual_priority, payer,
    powerful, immediate, constrained, global).

% Supports a comprehensive approach to AI safety, ensuring that both future and present harms are considered. Benefits from a broader mandate but may find it difficult to prioritize specific interventions or communicate a unified message.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, public_advocacy_groups_dual_priority, beneficiary,
    organized, generational, mobile, global).

% Primarily concerned with preventing catastrophic, extinction-level AI risks. Views the dual-priority approach as potentially diluting focus and resources from the most critical threat, but is structurally included in the broader conversation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_focused_researchers, excluded,
    organized, civilizational, identity_locked, global).

% Primarily concerned with mitigating immediate, documented harms from AI systems. Views the dual-priority approach as potentially diverting attention from urgent, actionable problems, but is structurally included in the broader conversation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_focused_advocates, excluded,
    organized, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse stakeholders in the AI safety community to address a broad spectrum of risks, preventing a fragmented approach where different groups only focus on their preferred risk type.
% TRANSFER_FUNCTION: Transfers attention, funding, and policy focus towards a more integrated understanding of AI risks, from those who would exclusively prioritize one type of risk to a more balanced, comprehensive agenda.
% ABSENT_VOICES: While this reading attempts to include both perspectives, those who believe one risk type (e.g., existential) is overwhelmingly more important or urgent than the other may feel their specific concerns are diluted or deprioritized within the 'dual priority' framework.
% DISAPPEARANCE_RATIONALE: If the commitment to dual priority vanished, the AI safety field would likely fragment into two distinct, potentially competing, camps: one focused solely on existential risk and another on near-term harms. Resource allocation, policy advocacy, and research agendas would diverge significantly.
% FOUNDING_PROBLEM: The AI safety field was becoming polarized, with 'x-risk' and 'AI ethics' communities often operating in silos, competing for resources and public attention, and failing to present a unified front on AI governance.
% FOUNDING_PROBLEM_CORROBORATION: Leading AI safety organizations, interdisciplinary academic initiatives, and international policy forums attest to the ongoing challenge of integrating diverse risk perspectives. Reports from the UN, OECD, and various national AI strategies corroborate the need for a comprehensive approach to avoid fragmentation.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).
:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the cost of broad coordination and the potential for diluted focus. Suppression (0.3) is low, as this is a normative commitment rather than a coercive structure, but it does exert pressure on stakeholders to adopt a comprehensive view. Theater ratio (0.2) is also low, as the commitment is genuinely aimed at addressing risks, though some performative aspects may arise from the difficulty of simultaneously prioritizing diverse concerns. Resistance (0.5) is moderate, as some stakeholders still prefer a more focused approach.
 *
 * PERSPECTIVAL GAP:
 *   While this reading aims for unity, individual researchers or advocates may still perceive a tension between the two priorities, leading to internal conflicts over resource allocation or strategic direction. The engine's per-seat classification would reflect this by showing different directionalities for those who feel their specific focus is being 'paid for' by the broader mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   AI safety researchers and policy makers who champion this dual-priority approach are beneficiaries, as it legitimizes their comprehensive agenda. AI developers are payers, as they bear the cost of implementing a wider range of safety measures. Existential-risk and near-term-harms focused groups are 'excluded' in the sense that their singular focus is not the primary agenda, but they are still part of the broader conversation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_coherence,
    'Can resources (funding, research talent, policy attention) be effectively allocated to both existential and near-term AI risks simultaneously without one implicitly cannibalizing the other?',
    'Empirical studies of funding flows and research output in organizations adopting a dual-priority mandate; analysis of policy outcomes in jurisdictions implementing comprehensive AI safety regulations.',
    'If resource allocation proves incoherent, the ''dual priority'' claim may function as a ''tangled rope'' or ''snare'' by creating an illusion of comprehensive action while effectively deprioritizing one area, or by creating a ''piton'' if the effort becomes purely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_coherence, empirical, 'Whether the dual-priority commitment can be practically implemented without implicit trade-offs.').

omega_variable(
    framing_vs_action,
    'Is the ''dual priority'' framing primarily a rhetorical strategy to unify disparate groups, or does it genuinely lead to integrated and effective safety interventions across both risk categories?',
    'Qualitative analysis of policy documents and organizational strategies for concrete, measurable actions taken to address both risk types, rather than just rhetorical inclusion.',
    'If primarily rhetorical, the ''theater_ratio'' would be higher, and the constraint might drift towards a ''piton'' or ''snare'' if it serves to obscure inaction on one or both fronts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_vs_action, conceptual, 'Distinguishing between rhetorical commitment and actionable integration of dual priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__dual_priority_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__dual_priority_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__dual_priority_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__dual_priority_reading, base_extractiveness, 10, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__dual_priority_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__dual_priority_reading, suppression_requirement, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_safety_commitment' kernel, focusing on the integration of both existential and near-term AI risks. It is linked to sibling readings that prioritize one risk type over the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
