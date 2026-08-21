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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Unified AI Risk Governance Framework (Bridge Reading)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
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
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, 'de3fc412-f143-481a-b04d-e02e02c6bd91').
narrative_ontology:cs_kernel_codification('de3fc412-f143-481a-b04d-e02e02c6bd91', distributed).
narrative_ontology:cs_authority_grounding('de3fc412-f143-481a-b04d-e02e02c6bd91', expertise).
narrative_ontology:cs_interpretation_layer_present('de3fc412-f143-481a-b04d-e02e02c6bd91').
narrative_ontology:cs_reading_relation('de3fc412-f143-481a-b04d-e02e02c6bd91', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('de3fc412-f143-481a-b04d-e02e02c6bd91', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('de3fc412-f143-481a-b04d-e02e02c6bd91', foundational, risks_are_structurally_entangled).
narrative_ontology:cs_axiom_status(risks_are_structurally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('de3fc412-f143-481a-b04d-e02e02c6bd91', risks_are_structurally_entangled, empirically_contingent).
narrative_ontology:cs_axiom('de3fc412-f143-481a-b04d-e02e02c6bd91', foundational, unified_governance_is_optimal).
narrative_ontology:cs_axiom_status(unified_governance_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('de3fc412-f143-481a-b04d-e02e02c6bd91', unified_governance_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('de3fc412-f143-481a-b04d-e02e02c6bd91', integrated_risk_management_paradigm).
narrative_ontology:cs_drift_state('de3fc412-f143-481a-b04d-e02e02c6bd91', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('de3fc412-f143-481a-b04d-e02e02c6bd91', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., interdisciplinary research centers, policy think tanks) benefit from funding and legitimacy by advocating for and developing unified frameworks that integrate both near-term and existential AI risks. Their influence depends on maintaining this bridge.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, beneficiary,
    organized, biographical, constrained, global).

% Academics and practitioners who specialize in interdisciplinary approaches to AI safety and ethics. They benefit from the intellectual space and funding opportunities created by the push for unified frameworks, allowing their work to be recognized as central to the field.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, integrated_researchers, beneficiary,
    moderate, biographical, constrained, global).

% Bear the present harms of AI systems (bias, discrimination, surveillance, labor displacement). While the bridge reading acknowledges these, the practical implementation of unified frameworks may still dilute immediate action or divert resources, making them indirect payers.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, marginalized_populations, payer,
    powerless, immediate, trapped, local).

% Are the primary targets of existential risk mitigation efforts. While the bridge reading aims to protect them, the inherent uncertainty and long time horizons mean that current resource allocation might still be insufficient or misdirected, making them indirect payers if the unified approach fails to adequately address long-term threats.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Advocate for immediate action on present AI harms. While their concerns are theoretically integrated into the bridge reading, they often feel their issues are deprioritized or diluted by the focus on abstract, long-term risks, leading to a sense of exclusion from effective policy-making.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_advocates, excluded,
    organized, biographical, constrained, national).

% Focus primarily on preventing catastrophic AI scenarios. While their concerns are integrated, they may view the bridge reading as insufficiently urgent or as diverting critical resources from the most pressing long-term threats, leading them to operate in parallel or to critique the unified approach.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_prioritizers, excluded,
    powerful, generational, mobile, global).

% Are tasked with developing and implementing AI governance policies. They seek frameworks that can address the full spectrum of risks to satisfy diverse constituencies, but face challenges in balancing competing priorities and allocating limited resources effectively.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate disparate research and policy communities (near-term ethics, long-term safety) by providing a common conceptual language and framework for addressing AI risks, preventing fragmentation and ensuring a holistic approach.
% TRANSFER_FUNCTION: Transfers legitimacy, funding, and intellectual capital to interdisciplinary research and policy initiatives that integrate both near-term and existential AI risk concerns, from more siloed approaches.
% ABSENT_VOICES: Advocates for either pure near-term or pure existential risk prioritization, who feel their specific concerns are diluted or deprioritized within a unified framework, are often marginalized in the discourse, leading them to form separate advocacy groups or research silos.
% DISAPPEARANCE_RATIONALE: If the bridge reading vanished, the AI governance landscape would likely revert to a more fragmented state, with near-term and existential risk communities operating in greater isolation, potentially leading to less comprehensive policy and research efforts.
% FOUNDING_PROBLEM: The AI risk discourse was becoming increasingly polarized, with near-term and existential risk communities often working in silos, competing for resources, and failing to recognize the structural entanglement of their concerns, leading to incomplete or contradictory governance proposals.
% FOUNDING_PROBLEM_CORROBORATION: Academic reviews of AI ethics and safety literature, interdisciplinary workshops, and policy reports from neutral bodies (e.g., UN agencies, non-partisan think tanks) consistently highlight the need for integrated approaches to overcome fragmentation and polarization in the field.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).
:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unified_framework_efficacy,
    'Does a unified framework genuinely lead to more effective mitigation of both near-term harms and existential risks, or does it dilute focus and resources?',
    'Empirical studies comparing policy outcomes and resource allocation in unified vs. siloed governance approaches over a 5-10 year period.',
    'If unified frameworks prove ineffective or diluting, the constraint''s extractiveness for victims (marginalized populations, future generations) would be higher, potentially reclassifying it as a Tangled Rope or even Snare if the coordination story becomes pure cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unified_framework_efficacy, empirical, 'Assesses the practical effectiveness of the bridge reading''s core premise.').

omega_variable(
    resource_allocation_bias,
    'Is the resource allocation within unified frameworks genuinely balanced between near-term and existential risks, or does it subtly favor one over the other?',
    'Detailed analysis of funding flows, research priorities, and policy implementation within institutions adopting unified frameworks, disaggregated by risk type.',
    'If resources are disproportionately allocated, the directionality for the disfavored victim group would shift towards higher extraction, indicating a hidden bias within the ''unified'' approach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_bias, empirical, 'Examines whether resource distribution reflects the claimed balance.').

omega_variable(
    framing_underdetermination,
    'Is the ''structural entanglement'' of risks a genuine empirical fact, or a conceptual framing chosen to facilitate coordination between disparate communities?',
    'Philosophical and scientific analysis of the causal links between near-term AI harms and long-term existential risks. If the links are weak or non-existent, the ''entanglement'' is a conceptual bridge, not a structural one.',
    'If entanglement is primarily a conceptual framing, the constraint''s legitimacy as a ''rope'' (solving a genuine coordination problem) would be weaker, and its extractiveness for those whose concerns are diluted would be higher, as the coordination story would be less grounded in reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'Distinguishes between empirical and conceptual grounding of risk entanglement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__bridge_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__bridge_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__bridge_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
