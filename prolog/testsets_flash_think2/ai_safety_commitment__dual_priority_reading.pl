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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: AI Safety: Dual Priority (Existential Risk & Near-Term Harms)
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the commitment within the AI safety community
 *   and policy circles to address both existential risks (long-term,
 *   catastrophic) and near-term harms (present-day, societal) as equally
 *   important, non-competing priorities. This reading attempts to unify
 *   disparate factions but faces inherent challenges in resource allocation
 *   and maintaining genuine balance, often leading to a 'tangled rope'
 *   dynamic where some are coordinated while others bear the cost of diluted
 *   focus.
 *
 * KEY AGENTS:
 *   - Dual Priority Advocates: Primary agenda-setters, powerful, global.
 *   - Existential Risk Researchers: Payers, powerful, global.
 *   - Near-Term Harms Researchers: Payers, powerful, global.
 *   - AI Developers: Beneficiaries, powerful, global.
 *   - General Public: Beneficiary, powerless, universal.
 *   - Resource Allocators: Agenda-setters, institutional, global.
 *   - Single-Focus X-Risk Advocates: Excluded, organized, global.
 *   - Single-Focus Near-Term Harms Advocates: Excluded, organized, global.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.65).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.55).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety: Dual Priority (Existential Risk & Near-Term Harms)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'fcbb021c-174a-4600-9a77-acbc6e2e9b81').
narrative_ontology:cs_kernel_codification('fcbb021c-174a-4600-9a77-acbc6e2e9b81', distributed).
narrative_ontology:cs_authority_grounding('fcbb021c-174a-4600-9a77-acbc6e2e9b81', practice).
narrative_ontology:cs_interpretation_layer_present('fcbb021c-174a-4600-9a77-acbc6e2e9b81').
narrative_ontology:cs_reading_relation('fcbb021c-174a-4600-9a77-acbc6e2e9b81', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('fcbb021c-174a-4600-9a77-acbc6e2e9b81', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('fcbb021c-174a-4600-9a77-acbc6e2e9b81', foundational, comprehensive_safety_imperative).
narrative_ontology:cs_axiom_status(comprehensive_safety_imperative, holdable).
narrative_ontology:cs_axiom_grounding('fcbb021c-174a-4600-9a77-acbc6e2e9b81', comprehensive_safety_imperative, deontological).
narrative_ontology:cs_axiom('fcbb021c-174a-4600-9a77-acbc6e2e9b81', secondary, resource_allocation_challenge).
narrative_ontology:cs_axiom_status(resource_allocation_challenge, holdable).
narrative_ontology:cs_axiom_grounding('fcbb021c-174a-4600-9a77-acbc6e2e9b81', resource_allocation_challenge, empirically_contingent).
narrative_ontology:cs_reference_frame('fcbb021c-174a-4600-9a77-acbc6e2e9b81', holistic_risk_management_framework).
narrative_ontology:cs_drift_state('fcbb021c-174a-4600-9a77-acbc6e2e9b81', contemporary_ai_policy_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fcbb021c-174a-4600-9a77-acbc6e2e9b81', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, dual_priority_advocates).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, general_public).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, single_focus_x_risk_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, single_focus_near_term_harms_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, resource_constrained_initiatives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_developers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and attempt to implement a comprehensive AI safety agenda that equally prioritizes both existential risks and near-term harms. They face the challenge of resource allocation and maintaining coherence across diverse research and policy communities.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, dual_priority_advocates, agenda_setter,
    powerful, generational, constrained, global).

% Focus primarily on preventing catastrophic, extinction-level outcomes from advanced AI. Under a dual-priority framework, they must compete for resources and attention with near-term concerns, potentially diluting their singular focus and perceived urgency.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_researchers, payer,
    powerful, generational, constrained, global).

% Address immediate, documented harms from deployed AI systems (e.g., bias, discrimination, labor displacement). Within a dual-priority framework, they must also compete for resources and attention with long-term, speculative risks, potentially diverting resources from urgent present-day problems.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers, payer,
    powerful, biographical, constrained, global).

% Benefit from a more stable and predictable regulatory environment that attempts to reconcile competing safety demands, potentially reducing the risk of sudden, disruptive policy shifts, even if it means adhering to broader safety guidelines.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_developers, beneficiary,
    powerful, biographical, mobile, global).

% Is the ultimate beneficiary of a truly comprehensive AI safety strategy that protects against both future catastrophic risks and present-day harms. However, the benefits are diffuse, and they bear indirect costs through resource allocation debates and potential delays in AI development.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, general_public, beneficiary,
    powerless, generational, trapped, universal).

% Government agencies, philanthropic organizations, and research funders tasked with distributing resources for AI safety. They are under pressure to demonstrate commitment to both priorities, often leading to difficult trade-offs and internal conflicts.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, resource_allocators, agenda_setter,
    institutional, biographical, constrained, global).

% Believe that existential risk is the paramount concern, requiring singular focus and maximal resource allocation. Their arguments for exclusive prioritization are often sidelined or diluted within a dual-priority framework, making them effectively excluded from the central policy-making table.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, single_focus_x_risk_advocates, excluded,
    organized, generational, constrained, global).

% Believe that present-day harms are the most urgent and tangible, requiring immediate and dedicated intervention. Their arguments for exclusive prioritization are often sidelined or diluted within a dual-priority framework, making them effectively excluded from the central policy-making table.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, single_focus_near_term_harms_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure AI safety efforts are comprehensive, preventing the neglect of either long-term catastrophic risks or immediate societal harms, and fostering a unified, coherent approach across diverse stakeholders.
% TRANSFER_FUNCTION: Transfers resources (funding, attention, talent) from purely single-focus initiatives towards a more balanced portfolio, and potentially from the general public (via taxes/regulation) to safety research and implementation.
% ABSENT_VOICES: Advocates for purely market-driven AI development, or those who believe safety is an emergent property of robust systems, would object to any coordinated safety effort, particularly one that attempts to balance such disparate concerns. Their voices are often marginalized in the safety discourse itself.
% DISAPPEARANCE_RATIONALE: If this commitment to dual priority vanished, AI safety efforts would likely fragment further into competing, single-focus camps, leading to unaddressed risks, inefficient resource allocation, and a less coherent regulatory and research landscape. The current fragile consensus would collapse.
% FOUNDING_PROBLEM: The fragmentation of the AI safety discourse into competing camps (existential risk vs. near-term harms), leading to inefficient resource allocation, a lack of comprehensive strategy, and political gridlock.
% FOUNDING_PROBLEM_CORROBORATION: Independent policy analysts, interdisciplinary academic bodies, and some government advisory groups attest to the ongoing challenge of balancing these priorities, often citing the political and funding divides. Reports from organizations like the UN and OECD frequently highlight the need for integrated risk frameworks.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `tangled_rope` classification reflects the genuine coordination function (attempting to unify safety efforts) combined with asymmetric extraction. Extraction is moderate-high (0.65) because the inherent tension between these two distinct risk categories, especially under resource scarcity, means that truly treating them as 'non-competing' is difficult, leading to inefficiencies or implicit prioritization that extracts from one side. Suppression (0.55) arises from the pressure to conform to the dual-priority narrative, marginalizing purely single-focus advocacy. Theater (0.40) exists as rhetoric often outpaces actual balanced resource allocation. Resistance (0.70) is high due to ongoing friction from both single-focus camps.
 *
 * PERSPECTIVAL GAP:
 *   The 'dual priority' is experienced differently by various stakeholders. Dual priority advocates perceive it as a necessary and beneficial coordination, while single-focus researchers often experience it as a dilution of their critical work and a forced compromise that extracts from their specific agenda. The general public benefits diffusely but lacks agency in the allocation debates.
 *
 * DIRECTIONALITY LOGIC:
 *   Dual priority advocates and resource allocators are beneficiaries/agenda-setters as they shape the discourse and resource distribution. AI developers benefit from a more stable, less fragmented policy landscape. Researchers from both the existential risk and near-term harms camps are payers, as their specific priorities are diluted and they must compete for resources within a framework that may not fully align with their core mission. Single-focus advocates are excluded, as their singular focus is actively suppressed by the dual-priority framing.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the dual-priority commitment as a pure 'rope' (simple coordination) by highlighting the inherent extraction and suppression arising from the difficulty of genuinely balancing these distinct priorities. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine intent to coordinate and address a broader range of risks. The 'tangled rope' captures the hybrid nature where coordination efforts inadvertently create costs for some participants due to the structural tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'Is this ''dual_priority_reading'' a genuinely coherent and implementable framework, or is it a rhetorical attempt to bridge fundamentally competing priorities?',
    'Empirical analysis of resource allocation patterns over time: if resources consistently skew towards one priority despite stated dual commitment, it suggests a rhetorical rather than structural reconciliation. Also, qualitative analysis of policy outcomes and research focus.',
    'If rhetorical, the constraint''s effective extractiveness and theater ratio are higher, and its coordination function is weaker, potentially reclassifying it closer to a ''snare'' or ''piton''. If genuinely coherent, it reinforces the ''tangled_rope'' or even ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Ambiguity of the dual-priority framework''s coherence and implementability.').

omega_variable(
    resource_allocation_coherence,
    'Can resources (funding, talent, attention) be truly allocated ''non-competitively'' across existential risk and near-term harms, or does scarcity inherently force prioritization?',
    'Detailed tracking of funding flows, research output, and policy implementation across both categories. If zero-sum dynamics consistently emerge, it indicates inherent competition.',
    'If resource allocation is inherently competitive, the ''non-competing'' aspect of the constraint is false, increasing effective extraction from the less-prioritized area and potentially shifting the classification towards a ''snare'' for that specific victim group.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_coherence, empirical, 'Whether resource scarcity undermines the ''non-competing'' claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of single-focus advocacy structural (e.g., funding mandates) or internalized (e.g., self-censorship to appear collaborative)?',
    'Post-policy-shift analysis: if single-focus advocacy re-emerges strongly when dual-priority mandates are relaxed, it suggests structural suppression. If it persists even after structural barriers are removed, it indicates internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — advocates carry the suppression with them after formal mandates are removed, making the constraint more resilient and extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for single-focus advocates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__dual_priority_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__dual_priority_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(ai_s_tr_t18, ai_safety_commitment__dual_priority_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(ai_s_tr_t24, ai_safety_commitment__dual_priority_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(ai_s_tr_t30, ai_safety_commitment__dual_priority_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__dual_priority_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__dual_priority_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(ai_s_be_t18, ai_safety_commitment__dual_priority_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(ai_s_be_t24, ai_safety_commitment__dual_priority_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(ai_s_be_t30, ai_safety_commitment__dual_priority_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__dual_priority_reading, suppression_requirement, 6, 0.51).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__dual_priority_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(ai_s_su_t18, ai_safety_commitment__dual_priority_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(ai_s_su_t24, ai_safety_commitment__dual_priority_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(ai_s_su_t30, ai_safety_commitment__dual_priority_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_safety_commitment' kernel, which also includes 'existential_risk_reading' and 'near_term_harms_reading'. This reading attempts to integrate both, facing challenges in resource allocation and coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
