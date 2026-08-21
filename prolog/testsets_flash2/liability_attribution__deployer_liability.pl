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
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer Primary Liability for AI Systems
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint defines a legal and regulatory framework where the
 *   primary liability for harms caused by AI systems is attributed to the
 *   'deployer' – the entity that integrates and operates the AI in a specific
 *   context. This reading emphasizes the deployer's control over the final
 *   application and decision-making, shifting risk away from upstream
 *   developers and foundation model providers. It is one reading of the
 *   broader 'liability_attribution' kernel, which also includes
 *   developer-centric and shared liability approaches.
 *
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
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '91fc1fb8-8faa-48b2-a891-143a5c50ad93').
narrative_ontology:cs_kernel_codification('91fc1fb8-8faa-48b2-a891-143a5c50ad93', formalized).
narrative_ontology:cs_authority_grounding('91fc1fb8-8faa-48b2-a891-143a5c50ad93', lineage).
narrative_ontology:cs_interpretation_layer_present('91fc1fb8-8faa-48b2-a891-143a5c50ad93').
narrative_ontology:cs_reading_relation('91fc1fb8-8faa-48b2-a891-143a5c50ad93', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('91fc1fb8-8faa-48b2-a891-143a5c50ad93', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('91fc1fb8-8faa-48b2-a891-143a5c50ad93', foundational, control_equals_responsibility).
narrative_ontology:cs_axiom_status(control_equals_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('91fc1fb8-8faa-48b2-a891-143a5c50ad93', control_equals_responsibility, deontological).
narrative_ontology:cs_axiom('91fc1fb8-8faa-48b2-a891-143a5c50ad93', foundational, deployment_context_is_primary).
narrative_ontology:cs_axiom_status(deployment_context_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('91fc1fb8-8faa-48b2-a891-143a5c50ad93', deployment_context_is_primary, conventional).
narrative_ontology:cs_reference_frame('91fc1fb8-8faa-48b2-a891-143a5c50ad93', deployer_control_paradigm).
narrative_ontology:cs_drift_state('91fc1fb8-8faa-48b2-a891-143a5c50ad93', contemporary_ai_complexity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91fc1fb8-8faa-48b2-a891-143a5c50ad93', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, ai_developers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, ai_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, affected_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations or individuals who integrate and operate AI systems in specific contexts. They bear the primary legal and financial burden for harms caused by the AI system, including due diligence for opaque models. Their exit options are limited by the need to adopt AI for competitive advantage or public service mandates.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_deployers, payer,
    organized, biographical, constrained, global).

% Entities that design, build, and train AI models. Under this regime, they externalize significant deployment-related risks and costs to deployers, focusing on model development rather than downstream liability. They benefit from reduced legal exposure and compliance burdens.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_developers, beneficiary,
    institutional, generational, arbitrage, global).

% Specialized developers providing large, general-purpose AI models. They are largely shielded from downstream harms, as deployers assume primary liability for the specific applications of their models. This allows them to focus on scaling and distributing their core technology.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals or groups harmed by AI systems. While deployers are liable, the burden of proof and the complexity of attribution often fall on the affected public, making redress difficult. They bear the direct impact of AI failures with limited recourse against upstream actors.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, affected_public, payer,
    powerless, immediate, trapped, local).

% Government bodies responsible for establishing and enforcing AI liability frameworks. They define the scope of deployer liability and oversee compliance, aiming to incentivize responsible deployment while managing innovation. They actively enforce the attribution rules.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulators, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the implications of different liability regimes, identifying gaps, unintended consequences, and potential for regulatory capture. They provide critical commentary on the effectiveness and fairness of the deployer-centric approach.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ascribes clear responsibility for AI harms to the party with direct control over deployment context, aiming to incentivize risk mitigation at the point of use and streamline legal processes for victims.
% TRANSFER_FUNCTION: Transfers the primary burden of liability, risk assessment, and due diligence for AI harms from developers and foundation model providers to the deployers of AI systems. It also transfers the burden of navigating complex legal attribution to affected individuals.
% ABSENT_VOICES: Victims' advocates would argue for a more accessible and less burdensome path to redress, potentially advocating for strict liability or joint-and-several liability across the value chain. Small and medium-sized AI deployers would argue for a more nuanced approach that considers their limited resources compared to large developers.
% DISAPPEARANCE_RATIONALE: If deployer liability vanished, the AI industry would face a chaotic legal landscape. Developers would likely face increased direct liability, leading to higher costs, slower innovation, and a scramble to re-allocate risk. Deployers would lose a clear framework for responsibility, potentially leading to less cautious deployment or a complete halt in AI adoption due to unquantifiable risk. The legal system would struggle with attribution, and victims would have no clear path to redress.
% FOUNDING_PROBLEM: The problem of assigning responsibility for harms caused by complex, opaque AI systems, where multiple actors contribute to the value chain and traditional product liability frameworks are insufficient.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and legal scholars attest that the problem of AI liability is still live and evolving. While developers and foundation model providers benefit from this specific solution, the broader legal community acknowledges the ongoing challenge of fair and effective attribution in AI systems.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial because deployers bear significant costs for due diligence, risk management, and potential legal penalties, often for systems whose internal workings are opaque. Suppression (0.70) is high as deployers have limited options to avoid this liability without foregoing AI adoption, and affected parties face high barriers to challenging the attribution. The theater ratio (0.20) reflects that while some due diligence is genuine, a portion of compliance activity is performative, aimed at demonstrating adherence rather than truly mitigating all risks, especially for black-box models. The metrics show a trend of increasing extractiveness and suppression as the complexity and ubiquity of AI systems grow, making deployer liability a more significant burden over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of AI developers, this constraint is a reasonable coordination mechanism that allows them to innovate without being bogged down by every possible downstream use case. From the deployers' perspective, it's a highly extractive and suppressive mechanism that forces them to internalize risks they cannot fully control or understand. The affected public sees it as a barrier to justice. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   AI deployers are the primary targets (payers), bearing the costs and risks. AI developers and foundation model providers are the primary beneficiaries, externalizing significant liability. The affected public are also targets, as the framework often makes it difficult for them to seek redress. Regulators act as agenda-setters, defining and enforcing the terms of this liability.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_deployer_liability,
    'Is this constraint a fair and effective attribution of liability, or does it merely shift risk to the most accessible party?',
    'Empirical studies on the effectiveness of deployer liability in preventing AI harms and facilitating victim redress, compared to alternative liability models. Analysis of the actual control deployers have over opaque foundation models.',
    'If it''s found to be an unfair risk shift, the constraint would be reclassified as more extractive (closer to Snare) for deployers and less effective as a coordination mechanism. If effective, its coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_deployer_liability, conceptual, 'This constraint is the ''deployer_liability'' reading of the ''liability_attribution'' kernel. Sibling readings include ''developer_liability'' and ''shared_liability''.').

omega_variable(
    opacity_due_diligence_burden,
    'Can deployers realistically perform adequate due diligence on opaque foundation models, or does this requirement create an impossible burden?',
    'Technical advancements in AI explainability and auditability, or regulatory mandates for model transparency from developers. Legal precedents on the standard of care for deployers of black-box systems.',
    'If the burden is impossible, the suppression and extractiveness for deployers are higher than measured, pushing the constraint closer to a Snare. If transparency improves, the constraint''s coordination function is enhanced, and extractiveness for deployers may decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_due_diligence_burden, empirical, 'The feasibility of deployers fulfilling due diligence obligations for opaque AI systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__deployer_liability, theater_ratio, 5, 0.15).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__deployer_liability, theater_ratio, 10, 0.18).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__deployer_liability, theater_ratio, 15, 0.19).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__deployer_liability, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(liab_be_t5, liability_attribution__deployer_liability, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(liab_be_t10, liability_attribution__deployer_liability, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(liab_be_t15, liability_attribution__deployer_liability, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(liab_be_t20, liability_attribution__deployer_liability, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(liab_su_t5, liability_attribution__deployer_liability, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(liab_su_t10, liability_attribution__deployer_liability, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(liab_su_t15, liability_attribution__deployer_liability, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(liab_su_t20, liability_attribution__deployer_liability, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
