% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Shared Liability for AI Systems (Causal Contribution & Control)
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint establishes a legal framework for shared liability across
 *   the AI value chain, attributing responsibility based on causal
 *   contribution to harm and control over the AI system's operation. It aims
 *   to distribute risk more equitably than single-point liability models, but
 *   introduces new coordination costs and complexities in attribution. This
 *   is one reading (shared_liability) of the broader 'liability_attribution'
 *   kernel, which also includes developer_liability and deployer_liability
 *   readings.
 *
 * KEY AGENTS:
 *   - ai_developers: Payer (powerful/constrained) — bears costs of potential liability and increased compliance/insurance.
 *   - ai_deployers: Payer (powerful/constrained) — bears costs of potential liability and increased compliance/insurance.
 *   - affected_parties: Beneficiary (powerless/constrained) — benefits from clearer avenues for redress in case of harm.
 *   - insurance_providers: Beneficiary (institutional/arbitrage) — profits from new markets for AI liability insurance.
 *   - regulatory_bodies: Agenda Setter (institutional/analytical) — defines and enforces the liability framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.45).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.3).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.45).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Shared Liability for AI Systems (Causal Contribution & Control)").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '9d257570-bc41-42df-9c45-24a5b5aa7e54').
narrative_ontology:cs_kernel_codification('9d257570-bc41-42df-9c45-24a5b5aa7e54', formalized).
narrative_ontology:cs_authority_grounding('9d257570-bc41-42df-9c45-24a5b5aa7e54', lineage).
narrative_ontology:cs_interpretation_layer_present('9d257570-bc41-42df-9c45-24a5b5aa7e54').
narrative_ontology:cs_reading_relation('9d257570-bc41-42df-9c45-24a5b5aa7e54', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('9d257570-bc41-42df-9c45-24a5b5aa7e54', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('9d257570-bc41-42df-9c45-24a5b5aa7e54', foundational, liability_proportional_to_causal_contribution_and_control).
narrative_ontology:cs_axiom_status(liability_proportional_to_causal_contribution_and_control, holdable).
narrative_ontology:cs_axiom_grounding('9d257570-bc41-42df-9c45-24a5b5aa7e54', liability_proportional_to_causal_contribution_and_control, deontological).
narrative_ontology:cs_axiom('9d257570-bc41-42df-9c45-24a5b5aa7e54', secondary, risk_distribution_incentivizes_safety).
narrative_ontology:cs_axiom_status(risk_distribution_incentivizes_safety, holdable).
narrative_ontology:cs_axiom_grounding('9d257570-bc41-42df-9c45-24a5b5aa7e54', risk_distribution_incentivizes_safety, instrumental).
narrative_ontology:cs_reference_frame('9d257570-bc41-42df-9c45-24a5b5aa7e54', distributed_justice_framework).
narrative_ontology:cs_drift_state('9d257570-bc41-42df-9c45-24a5b5aa7e54', contemporary_ai_complexity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9d257570-bc41-42df-9c45-24a5b5aa7e54', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, affected_parties).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_providers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Creates the underlying AI models and software. Bears a portion of liability based on their causal contribution to harm and control over the model's design. Must invest in explainability, safety-by-design, and liability insurance.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_developers, payer,
    powerful, biographical, constrained, global).

% Integrates and operates AI systems in specific contexts. Bears a portion of liability based on their causal contribution to harm and control over deployment decisions, monitoring, and human oversight. Must invest in robust testing, monitoring, and indemnification agreements.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_deployers, payer,
    powerful, biographical, constrained, global).

% Individuals or groups harmed by AI systems. Benefits from clearer and more robust legal avenues for seeking redress, as liability is distributed across multiple responsible parties, increasing the likelihood of successful claims.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, affected_parties, beneficiary,
    powerless, immediate, trapped, local).

% Offers new AI liability insurance products to developers and deployers. Profits from the increased demand for risk coverage driven by the shared liability framework, becoming a key intermediary in risk transfer.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Establishes, interprets, and enforces the legal framework for shared liability. Develops guidelines for causal attribution and control, and adjudicates disputes. Aims to balance innovation with public safety and fairness.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Analyzes the effectiveness, fairness, and unintended consequences of the shared liability framework. Publishes research and advises policymakers, contributing to the ongoing evolution of legal theory in AI governance.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, insurance_providers).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates risk distribution and incentivizes responsible development and deployment of AI systems by ensuring that all parties with causal contribution and control share in the liability, preventing single points of failure or blame-shifting.
% TRANSFER_FUNCTION: Transfers financial risk and compliance costs from individual developers/deployers to a shared pool, often mediated by insurance, and transfers the burden of seeking redress from affected parties to a more robust, multi-party legal framework.
% ABSENT_VOICES: Advocates for 'no-fault' compensation schemes for AI harm, who would argue that the current framework still places too high a burden on affected parties to prove causation and control, are largely absent from the legislative drafting process.
% DISAPPEARANCE_RATIONALE: If shared liability vanished, the AI industry would likely revert to fragmented, uncertain liability regimes, potentially stifling innovation due to unmanageable risk for individual actors, or leaving affected parties with no clear path to redress. Insurance markets for AI would collapse, and regulatory efforts would lose a key lever.
% FOUNDING_PROBLEM: The rapid development and deployment of complex AI systems created a 'liability gap' where existing legal frameworks struggled to attribute responsibility for harms, leading to insufficient incentives for safety and inadequate redress for victims.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, consumer advocacy groups, and some forward-thinking industry leaders corroborate that the liability gap remains a live problem, even with shared liability frameworks, due to the evolving nature of AI and the difficulty of attribution. The problem is not fully 'solved' but is being actively addressed by this constraint.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).
:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the costs imposed on both developers and deployers for compliance, insurance, and potential damages, but also the benefit of distributed risk. Suppression (0.30) is relatively low, as the constraint aims to enable, rather than prevent, innovation, but it does suppress unilateral risk-taking. Theater ratio (0.10) is low, as the primary function is genuine risk distribution, though some performative compliance may exist. Accessibility collapse (0.60) is moderate, as it creates new hurdles for market entry but doesn't eliminate the ability to develop or deploy AI. Resistance (0.25) is moderate, as both developers and deployers push back on increased costs, but also see benefits in a clearer, shared framework.
 *
 * PERSPECTIVAL GAP:
 *   AI developers and deployers experience this as a cost-imposing constraint, requiring new investments in risk management and legal counsel. Affected parties, however, see it as a beneficial mechanism for justice and redress. Insurance providers view it as a new market opportunity. Regulatory bodies see it as a necessary evolution of legal frameworks for emerging technologies.
 *
 * DIRECTIONALITY LOGIC:
 *   Affected parties are beneficiaries (d=0.0) as they gain new avenues for redress. Insurance providers are also beneficiaries (d=0.0) as they profit from new markets. AI developers and deployers are victims (d=1.0) as they bear the direct costs of liability and compliance. Regulatory bodies are agenda setters (d=0.5) as they administer the system, balancing societal benefit with industry impact.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling genuine risk distribution as pure extraction by ensuring that the costs are tied to a demonstrable coordination function (fairer allocation of risk, clearer redress for harm). It avoids becoming a Snare by distributing the burden rather than concentrating it, and avoids being a Piton by actively evolving with the technology, rather than becoming inert. The ongoing contestation over 'causal contribution' and 'control' is key to preventing mandatrophy, as it forces continuous re-evaluation of the constraint's fit with technological reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_contribution_measurement,
    'How reliably can ''causal contribution'' and ''control'' be measured and attributed in complex AI systems?',
    'Development of standardized forensic tools and methodologies for AI system analysis, coupled with legal precedent on attribution in specific cases.',
    'If attribution is consistently difficult, the constraint''s enforcement becomes arbitrary or performative, increasing extractiveness for victims and potentially shifting it towards a Snare. If reliable, it strengthens the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_measurement, empirical, 'Ambiguity in measuring causal contribution and control for liability attribution.').

omega_variable(
    shared_liability_vs_developer_deployer,
    'Is this ''shared liability'' reading of the liability_attribution kernel genuinely distinct from the developer_liability or deployer_liability readings, or does it merely represent a different weighting of their core premises?',
    'Analysis of legal frameworks: if shared_liability introduces novel legal duties or mechanisms not present in the other readings, it is distinct. If it only reallocates existing duties, it is a variant.',
    'If merely a variant, the underlying structural dynamics of extraction and coordination may be more accurately captured by one of the ''primary'' liability readings, with shared_liability acting as a modifier. If distinct, it represents a unique approach to risk distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shared_liability_vs_developer_deployer, conceptual, 'This constraint is one reading (shared_liability) of the liability_attribution kernel. Sibling readings (developer_liability, deployer_liability) would shift the primary victim set and alter the distribution of coordination costs and opacity burdens.').

omega_variable(
    opacity_burden_distribution,
    'Does the contractual distribution of opacity burden genuinely distribute risk, or does it disproportionately concentrate it on less powerful actors in the value chain?',
    'Empirical study of indemnification clauses and insurance market outcomes across different sizes of AI developers and deployers.',
    'If the burden concentrates on less powerful actors, the constraint''s effective extractiveness for those actors is higher than the base measure suggests, potentially pushing their seat towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_burden_distribution, empirical, 'Whether contractual opacity burden distribution is equitable or concentrates risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(liab_be_t5, liability_attribution__shared_liability, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(liab_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(liab_be_t15, liability_attribution__shared_liability, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(liab_su_t5, liability_attribution__shared_liability, suppression_requirement, 5, 0.23).
narrative_ontology:measurement(liab_su_t10, liability_attribution__shared_liability, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(liab_su_t15, liability_attribution__shared_liability, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, ai_system_safety_standards).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, ai_ethics_guidelines).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'liability_attribution' kernel. Other readings include 'developer_liability' and 'deployer_liability', which focus on single-point responsibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
