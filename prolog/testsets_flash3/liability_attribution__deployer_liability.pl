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
 *   This constraint story describes the 'deployer liability' reading of AI
 *   liability attribution, where the entity deploying an AI system bears
 *   primary responsibility for harms. This framework aims to clarify
 *   accountability but shifts significant risk and compliance burden onto
 *   deployers, while shielding developers and foundation model providers. The
 *   structural delta from other readings is that deployers enter the victim
 *   set, developers externalize deployment risk, and the opacity of AI models
 *   becomes a due diligence burden for deployers.
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
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer Primary Liability for AI Systems").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, '3b2be99e-c3c3-400f-9a4b-e338b60da002').
narrative_ontology:cs_kernel_codification('3b2be99e-c3c3-400f-9a4b-e338b60da002', formalized).
narrative_ontology:cs_authority_grounding('3b2be99e-c3c3-400f-9a4b-e338b60da002', lineage).
narrative_ontology:cs_interpretation_layer_present('3b2be99e-c3c3-400f-9a4b-e338b60da002').
narrative_ontology:cs_reading_relation('3b2be99e-c3c3-400f-9a4b-e338b60da002', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('3b2be99e-c3c3-400f-9a4b-e338b60da002', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('3b2be99e-c3c3-400f-9a4b-e338b60da002', foundational, control_implies_responsibility).
narrative_ontology:cs_axiom_status(control_implies_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('3b2be99e-c3c3-400f-9a4b-e338b60da002', control_implies_responsibility, deontological).
narrative_ontology:cs_axiom('3b2be99e-c3c3-400f-9a4b-e338b60da002', foundational, deployment_context_is_primary_risk_factor).
narrative_ontology:cs_axiom_status(deployment_context_is_primary_risk_factor, holdable).
narrative_ontology:cs_axiom_grounding('3b2be99e-c3c3-400f-9a4b-e338b60da002', deployment_context_is_primary_risk_factor, empirically_contingent).
narrative_ontology:cs_reference_frame('3b2be99e-c3c3-400f-9a4b-e338b60da002', traditional_product_liability_analogy).
narrative_ontology:cs_drift_state('3b2be99e-c3c3-400f-9a4b-e338b60da002', contemporary_ai_opacity_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b2be99e-c3c3-400f-9a4b-e338b60da002', '').
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

% Organizations or entities that integrate and operate AI systems in specific contexts. They bear the primary legal and financial burden for harms caused by the AI, requiring extensive due diligence, risk assessment, and potential insurance. Their control over deployment context is cited as the basis for this liability.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_system_deployers, payer,
    organized, biographical, constrained, national).

% Companies or teams that design and build AI models and systems. Under this regime, they are largely shielded from downstream deployment risks, externalizing much of the liability to deployers. They benefit from reduced legal exposure and potentially faster market adoption of their products.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_developers, beneficiary,
    institutional, generational, arbitrage, global).

% Entities that create and distribute large, general-purpose AI models. This reading of liability significantly reduces their direct responsibility for harms arising from specific applications of their models, shifting the burden to those who deploy them. They benefit from a clearer path to market without extensive downstream liability.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals who suffer harm (e.g., discrimination, privacy violations, economic loss) due to the deployment of AI systems. While they have a legal avenue for redress against deployers, the complexity and power asymmetry often make effective recourse difficult. They bear the initial impact of harm.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, affected_individuals, payer,
    powerless, immediate, trapped, local).

% Government agencies responsible for defining and enforcing AI liability frameworks. They establish the rules that assign primary liability to deployers, often in response to calls for clear accountability in AI governance. They actively enforce compliance and adjudicate disputes.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Academics and experts who analyze the implications of different liability regimes for AI. They critique the fairness, effectiveness, and economic impact of placing primary liability on deployers, often proposing alternative frameworks.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear point of accountability for AI-related harms, simplifying legal processes for affected parties by identifying a single primary responsible entity (the deployer) with direct control over the operational context.
% TRANSFER_FUNCTION: Transfers the primary legal and financial risk for AI harms from developers and foundation model providers to the entities that deploy and operate AI systems, along with the associated costs of due diligence and compliance.
% ABSENT_VOICES: Advocates for a more distributed or developer-centric liability model, who argue that developers hold critical information and design choices that deployers cannot fully audit, are often marginalized in policy discussions favoring deployer-centric approaches. Also, future AI systems with high autonomy might challenge the 'control' premise of deployer liability.
% DISAPPEARANCE_RATIONALE: If deployer liability vanished, the legal landscape for AI would become highly uncertain, leading to a significant chilling effect on AI adoption as no party would clearly bear responsibility. Developers would face increased pressure to internalize risks, and affected individuals would struggle to find clear avenues for redress, forcing a rapid re-evaluation of liability frameworks.
% FOUNDING_PROBLEM: The rapid proliferation of AI systems created a 'liability gap' where it was unclear who was responsible for harms, hindering innovation due to uncertainty and leaving victims without clear recourse.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and some legal experts attest that the problem of clear AI liability remains live, and this framework provides a necessary, albeit imperfect, solution. AI developers also corroborate the need for clarity, even if they benefit from this specific attribution. Affected individuals, while critical of the framework's impact, corroborate the existence of the underlying liability gap.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because deployers incur substantial costs for compliance, insurance, and potential damages, while developers benefit from reduced liability. Suppression (0.70) is also high, as deployers have limited options to avoid these burdens if they wish to use AI, and affected individuals face high barriers to effective redress. Theater ratio (0.20) is moderate; while there's genuine effort to establish accountability, some of the 'control' narrative may mask the inherent opacity and complexity of AI systems that deployers cannot fully mitigate.
 *
 * PERSPECTIVAL GAP:
 *   Deployers experience this as a highly extractive and suppressive constraint, forcing them to internalize risks they may not fully control. Developers, conversely, see it as a necessary coordination mechanism that clarifies their role and enables innovation by reducing their direct exposure to downstream harms. Affected individuals experience it as a complex, often insufficient, avenue for redress. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   AI system deployers and affected individuals are the primary targets (payers), bearing the costs and risks. AI developers and foundation model providers are the primary beneficiaries, externalizing much of the liability. Regulatory bodies act as agenda-setters, enforcing this specific attribution model. Legal scholars are observers, analyzing its effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_vs_opacity_ambiguity,
    'To what extent do deployers genuinely have ''control'' over AI systems, given the increasing opacity and complexity of foundation models, and how does this affect their ability to mitigate risks?',
    'Empirical studies on deployer-developer information asymmetry and the practical limits of due diligence for black-box AI systems. Legal precedents challenging the ''control'' premise in specific harm cases.',
    'If deployer control is found to be significantly limited by AI opacity, the constraint''s extractiveness on deployers would be re-evaluated as higher (more unfair), potentially shifting the claimed type towards Snare for deployers, and increasing pressure for shared or developer liability models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_vs_opacity_ambiguity, empirical, 'Ambiguity regarding the actual control deployers have over complex AI systems versus the legal attribution of control.').

omega_variable(
    innovation_vs_safety_tradeoff,
    'Does placing primary liability on deployers genuinely foster innovation by clarifying developer risk, or does it stifle AI adoption by making deployment too costly and risky for potential users?',
    'Longitudinal economic studies comparing AI adoption rates and innovation metrics in jurisdictions with deployer-centric liability versus those with alternative models. Surveys of deployer and developer sentiment regarding risk perception and investment.',
    'If it stifles adoption, the coordination function is undermined, and the constraint''s overall benefit to society (beyond developers) would be lower, potentially reclassifying it as a Snare or Tangled Rope with a weaker coordination story. If it fosters innovation without undue burden, the Rope aspect is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_vs_safety_tradeoff, empirical, 'The actual impact of deployer liability on AI innovation and adoption versus safety.').

omega_variable(
    liability_framing_underdetermination,
    'Is the ''deployer_liability'' framing the most defensible approach to AI liability, or do alternative framings (developer-centric, shared responsibility) offer a more equitable and effective distribution of risk and accountability?',
    'Conceptual analysis and comparative legal theory evaluating the normative justifications and practical consequences of different liability models against criteria like fairness, efficiency, and victim redress. Policy debates and legislative outcomes.',
    'If an alternative framing gains widespread acceptance, this ''deployer_liability'' reading would be re-evaluated as a less optimal or even unjust constraint, potentially shifting its classification towards a Snare from a broader societal perspective, even if it retains a coordination function for developers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_framing_underdetermination, conceptual, 'The choice of primary liability locus (deployer, developer, or shared) is a contested conceptual framing with significant structural consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__deployer_liability, theater_ratio, 5, 0.12).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__deployer_liability, theater_ratio, 10, 0.15).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__deployer_liability, theater_ratio, 15, 0.18).
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

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'liability_attribution' kernel, focusing on deployer responsibility. It is structurally distinct from 'developer_liability' and 'shared_liability' readings, which attribute responsibility differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
