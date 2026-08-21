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
 *   domain: AI Safety/Technology Governance/Risk Assessment
 *
 * SUMMARY:
 *   This constraint represents the 'dual priority' reading of the AI safety
 *   commitment, which asserts that both existential risks and near-term harms
 *   must be addressed as non-competing priorities. While framed as a
 *   necessary coordination, the practical challenges of resource allocation
 *   under scarcity and the inherent tensions between different intervention
 *   strategies mean it functions as a Tangled Rope. It attempts to coordinate
 *   diverse communities but extracts from those who advocate for a more
 *   focused approach to either risk category.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.7).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.65).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety: Dual Priority (Existential Risk & Near-Term Harms)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "AI Safety/Technology Governance/Risk Assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'a68e53df-e6cc-47ee-937f-8f288044c815').
narrative_ontology:cs_kernel_codification('a68e53df-e6cc-47ee-937f-8f288044c815', distributed).
narrative_ontology:cs_authority_grounding('a68e53df-e6cc-47ee-937f-8f288044c815', practice).
narrative_ontology:cs_interpretation_layer_present('a68e53df-e6cc-47ee-937f-8f288044c815').
narrative_ontology:cs_reading_relation('a68e53df-e6cc-47ee-937f-8f288044c815', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('a68e53df-e6cc-47ee-937f-8f288044c815', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('a68e53df-e6cc-47ee-937f-8f288044c815', foundational, all_ai_risks_are_interconnected).
narrative_ontology:cs_axiom_status(all_ai_risks_are_interconnected, holdable).
narrative_ontology:cs_axiom_grounding('a68e53df-e6cc-47ee-937f-8f288044c815', all_ai_risks_are_interconnected, empirically_contingent).
narrative_ontology:cs_axiom('a68e53df-e6cc-47ee-937f-8f288044c815', foundational, resource_allocation_is_not_zero_sum).
narrative_ontology:cs_axiom_status(resource_allocation_is_not_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('a68e53df-e6cc-47ee-937f-8f288044c815', resource_allocation_is_not_zero_sum, empirically_contingent).
narrative_ontology:cs_reference_frame('a68e53df-e6cc-47ee-937f-8f288044c815', holistic_risk_management_framework).
narrative_ontology:cs_drift_state('a68e53df-e6cc-47ee-937f-8f288044c815', contemporary_resource_scarcity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a68e53df-e6cc-47ee-937f-8f288044c815', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_advocates_dual_focus).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, general_public_future_generations).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, single_focus_x_risk_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, single_focus_near_term_harms_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, ai_developers_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These advocates promote the necessity of addressing both long-term existential risks and immediate societal harms from AI. They actively shape research agendas, funding priorities, and policy recommendations to reflect this dual focus, often mediating between more extreme positions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_advocates_dual_focus, agenda_setter,
    institutional, generational, constrained, global).

% Researchers primarily focused on existential risks (e.g., AI alignment, superintelligence safety) find their resources, funding, and public attention diluted by the dual-priority framing. They bear the cost of having to justify their focus within a broader, more diffuse agenda.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, single_focus_x_risk_researchers, payer,
    powerful, biographical, constrained, global).

% Advocates primarily focused on near-term harms (e.g., bias, discrimination, labor displacement) also experience dilution of resources and attention. They bear the cost of having to integrate their urgent concerns into a framework that also prioritizes speculative future risks.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, single_focus_near_term_harms_advocates, payer,
    powerful, biographical, constrained, global).

% The ultimate beneficiaries of a comprehensive AI safety approach that theoretically protects against all types of risks, both present and future. Their interests are represented by advocates, but they have no direct agency or exit options.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, general_public_future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Companies and individuals developing and deploying AI systems are subject to evolving safety guidelines and regulations stemming from both risk categories. They bear the cost of compliance and adapting their practices to a complex, dual-mandate safety framework.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_developers_deployers, payer,
    organized, immediate, mobile, global).

% Government bodies and international organizations tasked with AI governance. They attempt to formulate policies that address both risk types, navigating political pressures from different advocacy groups and the technical complexities of each domain.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_makers_regulators, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate diverse AI safety communities and policy efforts, ensuring that the full spectrum of AI risks (from immediate harms to existential threats) is addressed, preventing a zero-sum competition for resources and attention.
% TRANSFER_FUNCTION: Transfers resources (funding, research attention, policy focus) from a single-minded pursuit of one risk type to a broader, more distributed approach across both, aiming for a more holistic risk management strategy.
% ABSENT_VOICES: Those who believe one risk type is fundamentally more urgent or tractable, or that the two are inherently in conflict, are structurally marginalized in this framing. They would argue for a more focused, less diluted approach to their preferred risk domain.
% DISAPPEARANCE_RATIONALE: If this commitment to dual priority vanished, the AI safety field would likely fragment into two distinct, potentially competing, camps. Each would focus exclusively on their preferred risk type, leading to uncoordinated and potentially contradictory research and policy efforts, and a significant reorganization of funding and institutional structures.
% FOUNDING_PROBLEM: The fragmentation of the AI safety field into competing camps (existential risk vs. near-term harms), leading to inefficient resource allocation, a lack of unified policy strategy, and a perception of internal conflict that undermined broader legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Independent policy analysts, interdisciplinary research organizations, and some philanthropic funders attest to the ongoing challenge of integrating these perspectives, citing persistent funding silos, academic disputes, and the difficulty of translating dual-priority rhetoric into genuinely integrated action. This corroboration comes from outside the immediate dual-focus advocacy groups.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.70) reflects the opportunity costs and dilution of resources experienced by groups advocating for a single focus, as well as the overhead of maintaining a broad, often internally inconsistent, agenda. Suppression (0.65) is high because this framing actively discourages and politically marginalizes single-minded advocacy for either extreme, enforcing a 'both-and' rhetoric. The theater ratio (0.45) indicates a significant portion of activity is performative balancing, rather than genuinely integrated risk mitigation, as the practical difficulties of simultaneous, non-competing action are often downplayed. The increasing trend in all metrics over the interval reflects the growing difficulty of maintaining this dual focus as AI capabilities advance and resource competition intensifies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dual-priority advocates, this constraint is a necessary Rope, coordinating a fragmented field for the greater good. From the perspective of single-focus researchers and advocates, it is a Snare or Tangled Rope, diluting their efforts and extracting resources and attention from their urgent concerns. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Advocates for the dual-priority approach and the general public (who benefit from a theoretically comprehensive safety strategy) are the beneficiaries. However, the general public's benefit is diffuse and indirect. Researchers and advocates focused on either existential risk or near-term harms, as well as AI developers, are the targets, as their specific agendas and resources are diluted or constrained by the dual mandate. Policy makers are agenda-setters, attempting to implement this complex, often contradictory, commitment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_coherence,
    'Can resources genuinely be allocated to both existential risk and near-term harms without one implicitly extracting from the other, especially under conditions of scarcity?',
    'Empirical studies of funding flows, research output, and policy implementation over time, assessing whether resource distribution is truly additive or if it represents a zero-sum trade-off.',
    'If resources are found to be zero-sum, the ''non-competing'' claim is false, increasing the constraint''s effective extractiveness and pushing its classification closer to a Snare for the ''victims'' of diluted focus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_coherence, empirical, 'Whether resource allocation for dual priorities is truly non-competing or implicitly extractive.').

omega_variable(
    intervention_strategy_compatibility,
    'Are the intervention strategies for existential risk and near-term harms truly compatible and mutually reinforcing, or do they pull in different, potentially contradictory, directions?',
    'Comparative analysis of proposed solutions and their side effects: e.g., does a focus on rapid AI capabilities development (to achieve alignment) exacerbate near-term deployment risks?',
    'If strategies are found to be contradictory, the constraint''s coordination function is undermined, increasing its theater ratio and potentially reclassifying it as a Piton or Snare if the ''coordination'' is merely rhetorical cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_strategy_compatibility, conceptual, 'Compatibility of intervention strategies for different AI risk types.').

omega_variable(
    dual_priority_legitimacy,
    'Is the ''dual priority'' framing a genuine attempt at holistic risk management, or a political compromise designed to maintain broad coalition support by avoiding difficult prioritization choices?',
    'Analysis of internal policy documents, funding decisions, and public statements for consistency between stated priorities and actual resource allocation and strategic direction.',
    'If primarily a political compromise, the constraint''s theater ratio would be higher, and its classification would lean more towards a Piton (if function atrophies) or Snare (if it actively extracts from specific groups for political stability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_priority_legitimacy, preference, 'Underlying motivation for the dual-priority framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2020, ai_safety_commitment__dual_priority_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(ai_s_tr_t2022, ai_safety_commitment__dual_priority_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement(ai_s_tr_t2024, ai_safety_commitment__dual_priority_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement(ai_s_tr_t2026, ai_safety_commitment__dual_priority_reading, theater_ratio, 2026, 0.43).
narrative_ontology:measurement(ai_s_tr_t2028, ai_safety_commitment__dual_priority_reading, theater_ratio, 2028, 0.44).
narrative_ontology:measurement(ai_s_tr_t2030, ai_safety_commitment__dual_priority_reading, theater_ratio, 2030, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2020, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(ai_s_be_t2022, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(ai_s_be_t2024, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement(ai_s_be_t2026, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement(ai_s_be_t2028, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2028, 0.69).
narrative_ontology:measurement(ai_s_be_t2030, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2030, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2020, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(ai_s_su_t2022, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement(ai_s_su_t2024, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement(ai_s_su_t2026, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2026, 0.63).
narrative_ontology:measurement(ai_s_su_t2028, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2028, 0.64).
narrative_ontology:measurement(ai_s_su_t2030, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2030, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_safety_commitment' kernel, which decomposes into three distinct constraints: 'dual_priority_reading', 'existential_risk_reading', and 'near_term_harms_reading'. Each represents a different structural claim about AI safety priorities and has a distinct epsilon value and stakeholder structure. This reading attempts to integrate both, facing coherence challenges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
