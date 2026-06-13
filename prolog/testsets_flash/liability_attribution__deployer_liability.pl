% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__deployer_liability, []).

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
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer Primary Liability for AI Systems
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint establishes that deployers of AI systems bear primary
 *   liability for harms caused by those systems, based on their control over
 *   deployment context and decision authority. This reading of liability
 *   attribution shields AI developers and foundation model providers from
 *   downstream risks, shifting the burden of due diligence and risk
 *   management onto the deployers. The constraint is actively enforced
 *   through legal frameworks and regulatory bodies.
 *
 * KEY AGENTS:
 *   - ai_system_deployers: Primary target (institutional/constrained) — bears liability and due diligence burden
 *   - ai_developers: Primary beneficiary (institutional/arbitrage) — externalizes deployment risk
 *   - foundation_model_providers: Secondary beneficiary (institutional/arbitrage) — shielded from downstream harm
 *   - affected_individuals: Secondary target (powerless/trapped) — bears unmitigated harm if deployer fails
 *   - regulatory_bodies: Agenda setter (institutional/analytical) — enforces liability regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.65).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.7).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.65).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer Primary Liability for AI Systems").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '5c1b3487-76f3-48be-9020-3aa7191d9eec').
narrative_ontology:cs_kernel_codification('5c1b3487-76f3-48be-9020-3aa7191d9eec', formalized).
narrative_ontology:cs_authority_grounding('5c1b3487-76f3-48be-9020-3aa7191d9eec', lineage).
narrative_ontology:cs_interpretation_layer_present('5c1b3487-76f3-48be-9020-3aa7191d9eec').
narrative_ontology:cs_reading_relation('5c1b3487-76f3-48be-9020-3aa7191d9eec', liability_attribution__developer_liability, influences).
narrative_ontology:cs_reading_relation('5c1b3487-76f3-48be-9020-3aa7191d9eec', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('5c1b3487-76f3-48be-9020-3aa7191d9eec', foundational, control_implies_responsibility).
narrative_ontology:cs_axiom_status(control_implies_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('5c1b3487-76f3-48be-9020-3aa7191d9eec', control_implies_responsibility, deontological).
narrative_ontology:cs_axiom('5c1b3487-76f3-48be-9020-3aa7191d9eec', foundational, deployment_context_is_primary_risk_factor).
narrative_ontology:cs_axiom_status(deployment_context_is_primary_risk_factor, holdable).
narrative_ontology:cs_axiom_grounding('5c1b3487-76f3-48be-9020-3aa7191d9eec', deployment_context_is_primary_risk_factor, empirically_contingent).
narrative_ontology:cs_reference_frame('5c1b3487-76f3-48be-9020-3aa7191d9eec', traditional_product_liability_framework).
narrative_ontology:cs_drift_state('5c1b3487-76f3-48be-9020-3aa7191d9eec', contemporary_ai_complexity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5c1b3487-76f3-48be-9020-3aa7191d9eec', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, ai_developers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, ai_system_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, affected_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations or individuals who integrate and use AI systems in real-world contexts. They bear the primary legal and financial responsibility for any harms caused by the AI system during its operation, including due diligence for system understanding and risk mitigation.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_system_deployers, payer,
    institutional, biographical, constrained, global).

% Entities that design, build, and train AI models and systems. Under this liability regime, they are largely shielded from direct liability for harms occurring at the deployment stage, externalizing significant risk to deployers.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_developers, beneficiary,
    institutional, generational, arbitrage, global).

% Specialized AI developers who create large, general-purpose foundation models. They benefit from this regime by having their liability limited to the model's inherent flaws, rather than its misuse or contextual failures by deployers.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals who suffer harm (e.g., discrimination, financial loss, physical injury) due to the deployment of an AI system. Their recourse is primarily against the deployer, who may or may not be able to fully compensate for the harm.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, affected_individuals, payer,
    powerless, immediate, trapped, local).

% Government agencies and legal systems responsible for establishing, interpreting, and enforcing AI liability laws. They define the scope of deployer responsibility and adjudicate disputes, shaping the operational reality of this constraint.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, ai_developers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates risk management by assigning a clear point of accountability (the deployer) for AI system harms, aiming to incentivize careful deployment and due diligence.
% TRANSFER_FUNCTION: Transfers the primary financial and legal burden of AI-induced harms from AI developers and foundation model providers to AI system deployers.
% ABSENT_VOICES: Advocates for affected individuals and small AI deployers, who often lack the resources to contest liability assignments, would argue for a more equitable distribution of risk that considers the power asymmetry in the AI value chain.
% DISAPPEARANCE_RATIONALE: If deployer liability vanished, the AI industry would face a significant legal vacuum. Developers would likely face increased pressure for direct liability, and the entire risk management framework for AI deployment would need to be re-established, leading to substantial reorganization of legal and business practices.
% FOUNDING_PROBLEM: The problem of assigning responsibility for harms caused by complex, opaque AI systems, especially given the distributed nature of AI development and deployment.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and policymakers attest that assigning liability for AI harms remains a live and evolving problem, with ongoing debates about the most effective and equitable approaches. Industry bodies (from both developer and deployer sides) corroborate the existence of the problem, though they dispute the optimal solution.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__deployer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__deployer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__deployer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because deployers bear significant financial and reputational risk, often without full transparency into the AI systems they deploy. Suppression (0.70) is high due to legal mandates and the difficulty of transferring liability upstream. Theater ratio (0.20) is low, as the enforcement of liability is a genuine function, though some performative compliance may exist. Accessibility collapse (0.40) is moderate; deployers can choose not to deploy AI, but this may mean losing competitive advantage. Resistance (0.55) is moderate, with deployers advocating for shared liability models.
 *
 * PERSPECTIVAL GAP:
 *   AI developers and foundation model providers experience this as a beneficial coordination mechanism that fosters innovation by reducing their direct liability. Deployers, however, experience it as an extractive burden, forcing them to internalize risks they may not be equipped to manage. Affected individuals are victims of the system's failures, regardless of the liability allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers and foundation model providers are clear beneficiaries (d near 0.0) as they externalize significant risk. AI system deployers are targets (d near 1.0) as they bear primary liability. Affected individuals are also targets, as they suffer the harms. Regulatory bodies are agenda setters, enforcing the regime (d near 0.5, but with institutional power to shape the rules).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling a structural subsidy to developers as a neutral coordination of risk. By identifying deployers as victims and developers as beneficiaries, it highlights the asymmetric transfer of risk and cost, which is characteristic of a Tangled Rope rather than a pure Rope or Mountain. If the underlying problem of AI risk management were truly solved, the need for such a specific liability allocation might diminish, but for now, it actively shapes the AI ecosystem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a fair and efficient allocation of liability, or a structural subsidy to AI developers?',
    'Empirical analysis of harm distribution and innovation incentives under different liability regimes; legal precedent from jurisdictions adopting alternative liability models.',
    'If primarily a subsidy, the constraint''s effective extraction from deployers is higher than currently measured, and its classification shifts closer to Snare. If fair, it remains a Tangled Rope coordinating risk management.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''deployer_liability'' reading of the ''liability_attribution'' kernel. Sibling readings (''developer_liability'', ''shared_liability'') would shift the victim set and the locus of enforcement.').

omega_variable(
    opacity_due_diligence_burden,
    'To what extent does the inherent opacity of advanced AI systems (e.g., black-box models) make deployer due diligence an impossible or prohibitively costly burden?',
    'Technical advancements in AI explainability and interpretability; regulatory standards for ''reasonable'' deployer due diligence given system complexity.',
    'If opacity makes due diligence impossible, deployers are effectively trapped, increasing their directionality and the constraint''s effective extraction. If explainability tools become effective, deployer exit options improve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opacity_due_diligence_burden, empirical, 'The burden of understanding AI system internals falls on deployers, but model opacity may make this an unmeetable standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__deployer_liability, theater_ratio, 5, 0.17).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__deployer_liability, theater_ratio, 10, 0.19).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__deployer_liability, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(liab_be_t5, liability_attribution__deployer_liability, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(liab_be_t10, liability_attribution__deployer_liability, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(liab_be_t15, liability_attribution__deployer_liability, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(liab_su_t5, liability_attribution__deployer_liability, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(liab_su_t10, liability_attribution__deployer_liability, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(liab_su_t15, liability_attribution__deployer_liability, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, ai_innovation_incentives).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, ai_safety_standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
