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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Shared Liability for AI Systems (Causal Contribution & Control)
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint describes a legal and regulatory framework for shared
 *   liability in AI systems, where responsibility for harms is distributed
 *   along the value chain based on each actor's causal contribution and
 *   control over the AI's development and deployment. It aims to address the
 *   'liability gap' in complex AI systems by ensuring accountability and
 *   incentivizing risk mitigation, but also introduces significant
 *   coordination costs and potential for extraction through legal and
 *   insurance markets. This is one reading of the broader
 *   'liability_attribution' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.65).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.55).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.65).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Shared Liability for AI Systems (Causal Contribution & Control)").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, 'e1a235dd-a8c8-4d70-ad7f-574360f4c997').
narrative_ontology:cs_kernel_codification('e1a235dd-a8c8-4d70-ad7f-574360f4c997', formalized).
narrative_ontology:cs_authority_grounding('e1a235dd-a8c8-4d70-ad7f-574360f4c997', lineage).
narrative_ontology:cs_interpretation_layer_present('e1a235dd-a8c8-4d70-ad7f-574360f4c997').
narrative_ontology:cs_reading_relation('e1a235dd-a8c8-4d70-ad7f-574360f4c997', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('e1a235dd-a8c8-4d70-ad7f-574360f4c997', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('e1a235dd-a8c8-4d70-ad7f-574360f4c997', foundational, liability_proportional_to_causal_control).
narrative_ontology:cs_axiom_status(liability_proportional_to_causal_control, holdable).
narrative_ontology:cs_axiom_grounding('e1a235dd-a8c8-4d70-ad7f-574360f4c997', liability_proportional_to_causal_control, empirically_contingent).
narrative_ontology:cs_axiom('e1a235dd-a8c8-4d70-ad7f-574360f4c997', foundational, risk_should_be_distributed_equitably).
narrative_ontology:cs_axiom_status(risk_should_be_distributed_equitably, holdable).
narrative_ontology:cs_axiom_grounding('e1a235dd-a8c8-4d70-ad7f-574360f4c997', risk_should_be_distributed_equitably, deontological).
narrative_ontology:cs_reference_frame('e1a235dd-a8c8-4d70-ad7f-574360f4c997', proportional_justice_framework).
narrative_ontology:cs_drift_state('e1a235dd-a8c8-4d70-ad7f-574360f4c997', contemporary_ai_complexity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e1a235dd-a8c8-4d70-ad7f-574360f4c997', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, legal_professionals).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, regulators).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_deployers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop the underlying AI models and components. Under shared liability, they bear a portion of the liability proportional to their causal contribution and control over the AI's design, leading to increased compliance costs, legal fees, and insurance premiums. Exit means abandoning the AI market.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_developers, payer,
    organized, biographical, constrained, global).

% Integrate and operate AI systems in specific contexts. They bear a portion of the liability based on their control over deployment, data, and operational parameters. This increases their risk management overhead, legal exposure, and insurance costs. Exit means foregoing AI adoption.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_deployers, payer,
    organized, biographical, constrained, global).

% Are the ultimate recipients of AI services and potential victims of AI harms. They benefit from clearer avenues for redress and incentives for safer AI, but may indirectly bear costs through higher prices for AI products/services or reduced innovation in risk-averse areas.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, end_users, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, end_users, payer).

% Offer specialized AI liability insurance products. They benefit from the increased demand for risk coverage driven by distributed liability, collecting premiums and shaping risk mitigation practices through policy terms.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Provide legal counsel, litigation services, and regulatory compliance advice related to AI liability. They benefit from the increased complexity and demand for legal expertise in attributing and managing shared liability.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, legal_professionals, beneficiary,
    organized, biographical, mobile, national).

% Design, implement, and enforce AI liability frameworks. They benefit from a clearer mandate to ensure accountability and manage systemic risks, but face the challenge of defining and measuring 'causal contribution and control' in practice.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulators, agenda_setter,
    institutional, generational, analytical, national).

% Monitor the implementation and impact of liability frameworks, advocating for approaches that prioritize fairness, transparency, and accountability. They analyze whether shared liability effectively achieves its goals without unintended consequences.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_ethics_advocates, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, insurance_providers).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for distributing accountability for AI-induced harms across multiple actors in the value chain, based on their respective causal contributions and control points, thereby incentivizing risk mitigation at each stage and ensuring redress for victims.
% TRANSFER_FUNCTION: Transfers legal and financial risk, as well as associated costs (e.g., insurance premiums, legal fees, compliance overhead), from individual parties to a shared pool, and ultimately from victims of harm to those with demonstrable causal contribution and control over the AI system.
% ABSENT_VOICES: Small AI startups and individual researchers, who may lack the resources to navigate complex shared liability frameworks, are often excluded from the policy-making process, potentially leading to disproportionate compliance burdens that stifle their innovation.
% DISAPPEARANCE_RATIONALE: If shared liability vanished overnight, the legal landscape for AI harms would revert to either concentrated liability (e.g., solely on developers or deployers) or a significant liability gap, leaving victims uncompensated and reducing incentives for responsible AI development and deployment. The entire AI governance ecosystem would need to re-evaluate risk and accountability.
% FOUNDING_PROBLEM: The inherent complexity and opacity of advanced AI systems make it difficult to attribute harm to a single actor, leading to a 'liability gap' where victims of AI-induced harm cannot secure redress, and no single party is sufficiently incentivized to ensure safety.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, consumer advocacy groups, and international bodies (e.g., European Commission proposals for AI liability) consistently corroborate the ongoing problem of AI liability gaps and the need for clear attribution mechanisms. Industry groups, while concerned about compliance costs, also acknowledge the need for legal clarity.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) is substantial due to the high compliance costs, legal fees, and insurance premiums incurred by developers and deployers, which are ultimately passed on. Suppression (0.55) is moderate; while it suppresses unilateral action and forces coordination, it doesn't entirely prevent alternative approaches to risk management. The theater ratio (0.20) is low, indicating that the legal and regulatory processes are genuinely functional in attributing liability, even if complex. Accessibility collapse (0.45) is moderate, as it makes simple, unilateral liability difficult but doesn't entirely remove options for risk management. Resistance (0.50) is moderate, as industry players resist increased costs while acknowledging the need for clear rules.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of end-users and AI ethics advocates, shared liability is a necessary coordination mechanism to ensure accountability and safety in a complex domain. From the perspective of AI developers and deployers, it is a substantially extractive framework that imposes significant costs and administrative burdens, potentially stifling innovation. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers and deployers are primary payers, bearing direct costs and risks. End-users are beneficiaries of increased safety and redress, but also indirect payers through higher product costs. Insurance providers and legal professionals are clear beneficiaries, profiting from the increased demand for their services. Regulators act as agenda-setters, defining and enforcing the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope acknowledges both the genuine coordination function (distributing accountability, incentivizing safety) and the asymmetric extraction (high costs for industry, profits for legal/insurance sectors). It prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination benefits for society and victims). The 'live' status of the founding problem suggests it's not a Piton, though the complexity of implementation could lead to theatricality over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_contribution_measurement,
    'How reliably and consistently can ''causal contribution'' and ''control'' be measured and attributed in complex, opaque AI systems across different legal jurisdictions?',
    'Development of standardized forensic AI analysis tools and legal precedents that establish clear methodologies for attribution, or empirical studies on the consistency of judicial/regulatory decisions.',
    'If attribution is consistently difficult or arbitrary, the constraint''s effective extractiveness for developers/deployers increases (due to uncertainty and litigation costs), and its coordination function for safety decreases, pushing it closer to a Snare. If clear, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_measurement, empirical, 'Ambiguity in measuring causal contribution and control in AI systems.').

omega_variable(
    innovation_vs_safety_tradeoff,
    'Does the increased cost and complexity of shared liability disproportionately stifle innovation, particularly for smaller AI actors, outweighing the benefits of enhanced safety and accountability?',
    'Longitudinal economic studies comparing innovation rates and market entry for small vs. large AI firms in jurisdictions with and without shared liability frameworks, alongside qualitative assessments of regulatory burden.',
    'If innovation is significantly stifled, the constraint''s overall societal benefit (coordination) is reduced, and its effective extraction is amplified for the broader AI ecosystem, potentially shifting the classification towards a Snare. If the balance is maintained, it supports the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_vs_safety_tradeoff, preference, 'Balance between incentivizing AI safety through liability and potential negative impacts on innovation.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this ''shared_liability'' reading of the ''liability_attribution'' kernel genuinely distinct and implementable, or does it collapse into one of the concentrated liability readings (developer/deployer) in practice due to implementation challenges?',
    'Empirical analysis of legal outcomes and regulatory enforcement in jurisdictions adopting shared liability: if liability consistently concentrates on a single actor despite the framework, the reading is effectively overridden by practice.',
    'If it collapses, the ''shared_liability'' reading is effectively foreclosed by practical realities, and the dominant effective constraint becomes one of the concentrated liability types, with different beneficiary/victim sets and extractiveness profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether shared liability remains a distinct legal framework in practice or defaults to concentrated liability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__shared_liability, theater_ratio, 5, 0.18).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.2).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__shared_liability, theater_ratio, 15, 0.2).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(liab_be_t5, liability_attribution__shared_liability, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(liab_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(liab_be_t15, liability_attribution__shared_liability, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(liab_su_t5, liability_attribution__shared_liability, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(liab_su_t10, liability_attribution__shared_liability, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(liab_su_t15, liability_attribution__shared_liability, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, ai_safety_standards).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, ai_innovation_incentives).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'liability_attribution' kernel. It focuses on distributing liability based on causal contribution and control, in contrast to readings that concentrate liability on developers or deployers. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
