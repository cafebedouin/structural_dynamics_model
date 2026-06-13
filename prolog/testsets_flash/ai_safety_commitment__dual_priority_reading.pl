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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety: Dual Priority Approach (Existential Risk & Near-Term Harms)
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'dual priority' reading of the AI safety
 *   commitment, asserting that both existential risks (x-risk) and near-term
 *   harms (NTH) must be addressed as non-competing priorities. It aims to
 *   coordinate a broad range of stakeholders by acknowledging both concerns.
 *   However, in practice, resource allocation and strategic focus often
 *   create implicit competition, leading to a Tangled Rope dynamic where some
 *   stakeholders (e.g., those focused on NTH) may feel their priorities are
 *   extracted from or suppressed by the broader x-risk narrative, even as the
 *   overall framework provides coordination.
 *
 * KEY AGENTS:
 *   - ai_safety_researchers: Agenda setter/Beneficiary (institutional/arbitrage) — shapes the discourse, benefits from broad funding.
 *   - policy_makers: Agenda setter/Beneficiary (institutional/constrained) — implements regulations, gains legitimacy from addressing both concerns.
 *   - resource_constrained_researchers: Payer (moderate/constrained) — faces pressure to align with dual priorities, potentially diluting focus.
 *   - implementation_teams: Payer (organized/constrained) — bears the cost of integrating diverse safety requirements, often under conflicting directives.
 *   - affected_populations: Victim (powerless/trapped) — directly impacted by near-term harms, but their concerns may be diluted by x-risk focus.
 *   - public_trust_advocates: Beneficiary (organized/mobile) — benefits from a comprehensive safety narrative, but may find practical implementation challenging.
 *   - x_risk_maximalists: Excluded (powerful/constrained) — would argue for singular focus on existential risk, but are rhetorically sidelined by the dual-priority framing.
 *   - near_term_harms_activists: Excluded (organized/constrained) — would argue for singular focus on present-day harms, also sidelined by the dual-priority framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.6).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.4).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety: Dual Priority Approach (Existential Risk & Near-Term Harms)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '7f5857c8-91e1-48fb-8165-ed8e89dbd900').
narrative_ontology:cs_kernel_codification('7f5857c8-91e1-48fb-8165-ed8e89dbd900', formalized).
narrative_ontology:cs_authority_grounding('7f5857c8-91e1-48fb-8165-ed8e89dbd900', expertise).
narrative_ontology:cs_interpretation_layer_present('7f5857c8-91e1-48fb-8165-ed8e89dbd900').
narrative_ontology:cs_reading_relation('7f5857c8-91e1-48fb-8165-ed8e89dbd900', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f5857c8-91e1-48fb-8165-ed8e89dbd900', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('7f5857c8-91e1-48fb-8165-ed8e89dbd900', foundational, holistic_risk_management_is_optimal).
narrative_ontology:cs_axiom_status(holistic_risk_management_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('7f5857c8-91e1-48fb-8165-ed8e89dbd900', holistic_risk_management_is_optimal, instrumental).
narrative_ontology:cs_axiom('7f5857c8-91e1-48fb-8165-ed8e89dbd900', secondary, resource_allocation_can_be_non_competing).
narrative_ontology:cs_axiom_status(resource_allocation_can_be_non_competing, holdable).
narrative_ontology:cs_axiom_grounding('7f5857c8-91e1-48fb-8165-ed8e89dbd900', resource_allocation_can_be_non_competing, empirically_contingent).
narrative_ontology:cs_reference_frame('7f5857c8-91e1-48fb-8165-ed8e89dbd900', integrated_risk_governance).
narrative_ontology:cs_drift_state('7f5857c8-91e1-48fb-8165-ed8e89dbd900', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f5857c8-91e1-48fb-8165-ed8e89dbd900', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_researchers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, policy_makers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, public_trust_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, resource_constrained_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, implementation_teams).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, affected_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).

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
 *   The constraint is a Tangled Rope because it genuinely attempts to coordinate diverse concerns within the AI safety community (beneficiaries: AI safety researchers, policy makers, public trust advocates) but simultaneously creates asymmetric extraction. Resource-constrained researchers and implementation teams bear the cost of balancing potentially conflicting demands, and affected populations may find their immediate needs diluted by the long-term, speculative nature of x-risk. Active enforcement is required to maintain the 'non-competing' narrative and ensure resources are nominally distributed across both areas, preventing a full collapse into either a pure x-risk or pure NTH focus. The extractiveness (0.6) reflects the implicit trade-offs and dilution of focus, while suppression (0.4) indicates the pressure to conform to the dual-priority framing, even if it doesn't fully align with individual research or advocacy goals. Theater ratio (0.25) suggests some performative balancing acts, but a core coordination function remains.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of AI safety researchers and policy makers, this is a necessary and beneficial coordination mechanism, allowing for a holistic approach to AI safety. From the perspective of resource-constrained researchers, implementation teams, and especially affected populations, the 'dual priority' can feel like a dilution of focus, where immediate, tangible harms receive less attention or resources than existential, speculative risks, leading to a sense of extraction or suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   AI safety researchers and policy makers are beneficiaries (d=0.0-0.2) as they gain legitimacy and broader funding by encompassing both concerns. Resource-constrained researchers and implementation teams are payers (d=0.6-0.8) as they must navigate the complexities and potential conflicts of balancing these priorities. Affected populations are victims (d=0.9-1.0) as their immediate needs may be deprioritized or diluted. Public trust advocates are beneficiaries (d=0.1-0.3) as the dual-priority narrative fosters broader public acceptance. X-risk maximalists and near-term harms activists are excluded (d=0.9-1.0) as their singular focus is suppressed by the dual-priority framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'dual priority' framing is intended to prevent the AI safety mandate from atrophying into either a purely speculative or purely reactive endeavor. By explicitly linking both, it attempts to maintain a broad and relevant mandate. However, if the practical implementation consistently fails to balance resources, the 'non-competing' aspect could become a mandatrophy, where the stated goal (dual priority) is no longer met by the actual operation (implicit competition), leading to a performative constraint that extracts from the neglected priority's stakeholders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_coherence,
    'Can resources truly be allocated to both existential risk and near-term harms without one implicitly competing with or de-prioritizing the other, especially under scarcity?',
    'Empirical observation of funding flows, research agendas, and policy implementation over time; analysis of trade-offs made in practice.',
    'If resources consistently favor one priority, the ''dual priority'' claim becomes performative, and the constraint shifts towards a Snare for the neglected priority''s stakeholders. If balanced allocation is achieved, it reinforces the Rope aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_coherence, empirical, 'Assesses the practical feasibility of non-competing dual priorities in AI safety.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''dual_priority_reading'' of the ''ai_safety_commitment'' kernel. What would change if an alternative reading (e.g., ''existential_risk_reading'' or ''near_term_harms_reading'') were adopted?',
    'Analysis of policy shifts, funding reallocations, and changes in research focus if a different reading gains dominance.',
    'Adopting the ''existential_risk_reading'' would narrow the victim set to ''humanity'' and focus resources on long-term, speculative risks, potentially making the constraint a Snare for those facing immediate harms. Adopting the ''near_term_harms_reading'' would broaden the victim set to ''affected_populations'' and focus on present-day issues, potentially making the constraint a Snare for those concerned with future catastrophic risks. This ''dual_priority_reading'' attempts to balance, but faces internal coherence challenges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documents the specific reading of the AI safety commitment kernel and its implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__dual_priority_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__dual_priority_reading, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__dual_priority_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__dual_priority_reading, base_extractiveness, 10, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__dual_priority_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__dual_priority_reading, suppression_requirement, 10, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI safety commitment' kernel, which also includes 'existential_risk_reading' and 'near_term_harms_reading'. This reading attempts to integrate both, facing challenges in resource allocation and practical coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
