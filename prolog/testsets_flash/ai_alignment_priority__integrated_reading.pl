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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated AI Alignment Priority
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'integrated reading' of AI alignment
 *   priority, asserting that both catastrophic (existential) risks and
 *   present (near-term) harms must be addressed as complementary, not
 *   competing, priorities. It seeks to bridge the divide between different AI
 *   ethics communities by advocating for a holistic approach to risk
 *   assessment and mitigation. The constraint is framed as a 'rope' because
 *   it aims to coordinate diverse stakeholders towards a common, beneficial
 *   goal, with decreasing extractiveness and suppression over time as the
 *   integrated approach gains traction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.45).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.3).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated AI Alignment Priority").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '5b4205f8-0668-435b-a77c-590ca331864d').
narrative_ontology:cs_kernel_codification('5b4205f8-0668-435b-a77c-590ca331864d', distributed).
narrative_ontology:cs_authority_grounding('5b4205f8-0668-435b-a77c-590ca331864d', expertise).
narrative_ontology:cs_interpretation_layer_present('5b4205f8-0668-435b-a77c-590ca331864d').
narrative_ontology:cs_reading_relation('5b4205f8-0668-435b-a77c-590ca331864d', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b4205f8-0668-435b-a77c-590ca331864d', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('5b4205f8-0668-435b-a77c-590ca331864d', foundational, all_harm_scales_matter).
narrative_ontology:cs_axiom_status(all_harm_scales_matter, holdable).
narrative_ontology:cs_axiom_grounding('5b4205f8-0668-435b-a77c-590ca331864d', all_harm_scales_matter, deontological).
narrative_ontology:cs_axiom('5b4205f8-0668-435b-a77c-590ca331864d', foundational, interdependence_of_risks).
narrative_ontology:cs_axiom_status(interdependence_of_risks, holdable).
narrative_ontology:cs_axiom_grounding('5b4205f8-0668-435b-a77c-590ca331864d', interdependence_of_risks, empirically_contingent).
narrative_ontology:cs_reference_frame('5b4205f8-0668-435b-a77c-590ca331864d', holistic_risk_management).
narrative_ontology:cs_drift_state('5b4205f8-0668-435b-a77c-590ca331864d', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5b4205f8-0668-435b-a77c-590ca331864d', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_generations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, present_marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, ai_developers_and_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, existential_risk_advocates).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, nearterm_harms_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing integrated alignment strategies, balancing resources between long-term safety research and immediate harm mitigation. They face pressure from both existential risk and near-term ethics communities, and must navigate regulatory demands.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_developers_and_deployers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the long-term safety measures and catastrophic risk prevention efforts, ensuring a viable future. Their interests are represented by proxy through researchers and policymakers.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Benefit from the focus on mitigating immediate harms like bias, discrimination, and job displacement. Their advocacy pushes for ethical deployment and equitable access to AI benefits.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, present_marginalized_communities, beneficiary,
    organized, generational, constrained, global).

% Develop methodologies and frameworks for identifying and mitigating both catastrophic and present harms. They influence policy and industry best practices, advocating for a holistic approach to AI safety.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_ethics_researchers, agenda_setter,
    powerful, biographical, mobile, global).

% Bear the cost of resource diversion from pure existential risk research to include near-term harms. They view this as a necessary compromise for broader acceptance and legitimacy, but may perceive it as slowing down critical safety work.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, existential_risk_advocates, payer,
    organized, civilizational, constrained, global).

% Bear the cost of resource diversion from pure near-term harm mitigation to include long-term safety. They view this as a necessary compromise for broader acceptance and legitimacy, but may perceive it as slowing down critical justice work.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, nearterm_harms_advocates, payer,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse AI safety and ethics communities by establishing a shared framework that acknowledges and integrates both long-term catastrophic risks and immediate societal harms, preventing a zero-sum competition for resources and attention.
% TRANSFER_FUNCTION: Transfers resources (funding, research effort, policy attention) from a singular focus on either existential risk or near-term harms to a balanced portfolio addressing both. It also transfers legitimacy and broader public buy-in to the overall alignment effort.
% ABSENT_VOICES: Those who believe that AI alignment is an unsolvable problem or an unnecessary distraction from other global challenges are largely absent from this integrated discourse, as are those who prioritize unconstrained AI development above all ethical considerations.
% DISAPPEARANCE_RATIONALE: If this integrated approach vanished, the AI safety and ethics fields would likely fracture into competing factions, leading to uncoordinated efforts, resource misallocation, and a failure to address either class of harm effectively, potentially accelerating both catastrophic risks and present injustices.
% FOUNDING_PROBLEM: The AI alignment field was bifurcated, with 'long-termists' focusing on existential risk and 'short-termists' on present harms, leading to unproductive conflict, resource competition, and a lack of comprehensive strategy.
% FOUNDING_PROBLEM_CORROBORATION: Leading AI ethics organizations, interdisciplinary research institutes, and major philanthropic funders (outside of those exclusively focused on one extreme) corroborate that the problem of bifurcation and resource competition is still live, though the integrated approach has made progress. Academic publications and policy whitepapers also attest to this ongoing challenge.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).

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
 *   The extractiveness (0.45) is moderate, reflecting the necessary compromise and resource reallocation from previously siloed efforts. It's not zero because some parties (e.g., pure existential risk advocates) still perceive a 'cost' in broadening the focus. Suppression (0.30) is relatively low, as this reading aims to reduce, rather than enforce, the suppression of alternative viewpoints by integrating them. Theater ratio (0.20) is also low, indicating a genuine effort to implement comprehensive strategies rather than performative gestures. The decreasing trend in extractiveness, suppression, and theater ratio over the interval reflects the growing acceptance and efficacy of the integrated approach.
 *
 * PERSPECTIVAL GAP:
 *   While the integrated reading aims to reduce perspectival gaps, some tension remains. Pure existential risk advocates might perceive the focus on near-term harms as a distraction from the most critical threat, while pure near-term harms advocates might see long-term safety as too abstract or speculative. This constraint attempts to bridge these perspectives by demonstrating their interdependence.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers and deployers, along with AI ethics researchers, act as agenda-setters, shaping the implementation of this integrated approach. Future generations and present marginalized communities are the primary beneficiaries, as their long-term and immediate interests are explicitly prioritized. Advocates for purely existential risk or near-term harms are 'payers' in the sense that they must compromise their singular focus, but they also benefit from the broader legitimacy and effectiveness of the integrated approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_balance,
    'Is the actual allocation of resources (funding, research talent) truly balanced between catastrophic and present harms, or does one still implicitly dominate?',
    'Quantitative analysis of funding flows, research publication topics, and policy initiatives over time, disaggregated by harm type.',
    'If resources remain skewed, the ''integrated'' claim might be performative, and the constraint could reclassify towards a Tangled Rope or Snare, reflecting continued extraction from the under-prioritized harm category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_balance, empirical, 'Assessing whether resource allocation genuinely reflects an integrated approach.').

omega_variable(
    measurement_of_harm_integration,
    'Are the metrics and methodologies for assessing catastrophic and present harms genuinely integrated, or are they still treated as separate, potentially incommensurable, categories?',
    'Review of AI risk assessment frameworks and audit methodologies for evidence of truly unified metrics and interdisciplinary approaches.',
    'If integration is superficial, the constraint''s effectiveness in coordinating efforts is reduced, potentially leading to a Piton where the ''integrated'' label is maintained theatrically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_harm_integration, conceptual, 'Evaluating the depth of methodological integration for different harm types.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''integrated_reading'' genuinely distinct and stable, or does it risk collapsing back into one of the more extreme sibling readings under pressure?',
    'Longitudinal study of policy debates, funding priorities, and public discourse within the AI governance community to track shifts in emphasis.',
    'If the reading collapses, the constraint would be superseded by either the ''existential_risk_reading'' or ''nearterm_harms_reading'', leading to a reclassification reflecting that dominant priority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Stability of the integrated reading against pressures from sibling interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2020, ai_alignment_priority__integrated_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(ai_a_tr_t2022, ai_alignment_priority__integrated_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(ai_a_tr_t2024, ai_alignment_priority__integrated_reading, theater_ratio, 2024, 0.2).
narrative_ontology:measurement(ai_a_tr_t2026, ai_alignment_priority__integrated_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement(ai_a_tr_t2028, ai_alignment_priority__integrated_reading, theater_ratio, 2028, 0.15).
narrative_ontology:measurement(ai_a_tr_t2030, ai_alignment_priority__integrated_reading, theater_ratio, 2030, 0.12).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2020, ai_alignment_priority__integrated_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(ai_a_be_t2022, ai_alignment_priority__integrated_reading, base_extractiveness, 2022, 0.5).
narrative_ontology:measurement(ai_a_be_t2024, ai_alignment_priority__integrated_reading, base_extractiveness, 2024, 0.45).
narrative_ontology:measurement(ai_a_be_t2026, ai_alignment_priority__integrated_reading, base_extractiveness, 2026, 0.4).
narrative_ontology:measurement(ai_a_be_t2028, ai_alignment_priority__integrated_reading, base_extractiveness, 2028, 0.38).
narrative_ontology:measurement(ai_a_be_t2030, ai_alignment_priority__integrated_reading, base_extractiveness, 2030, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2020, ai_alignment_priority__integrated_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(ai_a_su_t2022, ai_alignment_priority__integrated_reading, suppression_requirement, 2022, 0.35).
narrative_ontology:measurement(ai_a_su_t2024, ai_alignment_priority__integrated_reading, suppression_requirement, 2024, 0.3).
narrative_ontology:measurement(ai_a_su_t2026, ai_alignment_priority__integrated_reading, suppression_requirement, 2026, 0.28).
narrative_ontology:measurement(ai_a_su_t2028, ai_alignment_priority__integrated_reading, suppression_requirement, 2028, 0.25).
narrative_ontology:measurement(ai_a_su_t2030, ai_alignment_priority__integrated_reading, suppression_requirement, 2030, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'integrated_reading' of the 'ai_alignment_priority' kernel, which also includes 'existential_risk_reading' and 'nearterm_harms_reading'. This reading aims to bridge the divide between the other two by treating both types of harms as complementary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
