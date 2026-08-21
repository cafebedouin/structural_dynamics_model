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
 *   This constraint describes a legal framework for shared liability in AI
 *   systems, where responsibility for harms is distributed among developers
 *   and deployers based on their causal contribution and control. It is one
 *   reading of the broader 'liability_attribution' kernel, which also
 *   includes readings that concentrate liability solely on developers or
 *   deployers. This 'shared_liability' reading aims to address the 'liability
 *   gap' in AI by ensuring accountability across the value chain, leading to
 *   increased coordination costs for industry but improved redress for the
 *   public.
 *
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
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Shared Liability for AI Systems (Causal Contribution & Control)").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '10e5fffb-8433-4c99-b3b1-00d7e667cbbd').
narrative_ontology:cs_kernel_codification('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', formalized).
narrative_ontology:cs_authority_grounding('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', lineage).
narrative_ontology:cs_interpretation_layer_present('10e5fffb-8433-4c99-b3b1-00d7e667cbbd').
narrative_ontology:cs_reading_relation('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', foundational, liability_proportional_to_causal_contribution_and_control).
narrative_ontology:cs_axiom_status(liability_proportional_to_causal_contribution_and_control, holdable).
narrative_ontology:cs_axiom_grounding('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', liability_proportional_to_causal_contribution_and_control, conventional).
narrative_ontology:cs_axiom('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', secondary, incentivize_responsible_innovation_via_distributed_risk).
narrative_ontology:cs_axiom_status(incentivize_responsible_innovation_via_distributed_risk, holdable).
narrative_ontology:cs_axiom_grounding('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', incentivize_responsible_innovation_via_distributed_risk, instrumental).
narrative_ontology:cs_reference_frame('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', distributed_accountability_framework).
narrative_ontology:cs_drift_state('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('10e5fffb-8433-4c99-b3b1-00d7e667cbbd', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, affected_public).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_providers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for the design and inherent capabilities of AI systems. Bears a portion of liability proportional to their causal contribution and control over the system's core functionality. Faces increased compliance costs and potential litigation.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_developers, payer,
    powerful, biographical, constrained, global).

% Responsible for integrating, configuring, and operating AI systems in specific contexts. Bears a portion of liability proportional to their causal contribution and control over the system's deployment environment and operational decisions. Faces increased due diligence and contractual overhead.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_deployers, payer,
    institutional, biographical, constrained, national).

% Benefits from a clearer path to redress for harms caused by AI systems, as liability is not solely concentrated on one party. Still faces the burden of proving harm and establishing causal links, but with a broader set of responsible parties.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, affected_public, beneficiary,
    powerless, immediate, trapped, local).

% Develops new insurance products (e.g., AI liability insurance) to cover the distributed risks for developers and deployers. Benefits from increased demand for these services and the ability to price risk based on clearer attribution rules. Also acts as a mechanism for risk transfer and opacity distribution.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_providers, beneficiary,
    organized, generational, mobile, global).

% Responsible for defining the legal framework for shared liability, establishing guidelines for causal contribution and control, and enforcing compliance. Aims to incentivize responsible AI development and deployment by distributing accountability.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for attributing responsibility for AI-related harms across the value chain, encouraging collaboration between developers and deployers on risk mitigation and transparency, and facilitating redress for victims.
% TRANSFER_FUNCTION: Transfers financial and reputational risk from the affected public to AI developers and deployers, distributed according to their respective causal contributions and control. Also transfers risk to insurance providers through premiums.
% ABSENT_VOICES: Small AI startups and open-source developers, who might face disproportionate compliance burdens and liability risks compared to larger entities, potentially stifling innovation. They would argue for safe harbors or tiered liability structures.
% DISAPPEARANCE_RATIONALE: If shared liability vanished, the legal landscape for AI harms would revert to a more fragmented state, likely concentrating liability on either developers or deployers, or leaving victims with no clear path to redress. This would significantly alter risk allocation, investment incentives, and public trust in AI.
% FOUNDING_PROBLEM: The 'black box' nature and complex interactions of AI systems make it difficult to attribute responsibility for harms to a single entity, leading to a 'liability gap' where victims struggle to obtain redress and no party is sufficiently incentivized to manage risk.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, consumer advocacy groups, and international policy organizations corroborate that the liability gap for AI harms remains a live and pressing problem, requiring clear attribution mechanisms to ensure accountability and foster public trust. Industry bodies also acknowledge the need for clarity, albeit with concerns about over-regulation.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates risk management and redress mechanisms (benefiting the public and insurance providers) while simultaneously extracting compliance costs and risk from developers and deployers. Extractiveness (0.45) reflects the significant, but distributed, burden on industry. Suppression (0.30) is moderate, as it relies on legal enforcement and contractual obligations rather than physical coercion, but it does suppress the option of avoiding liability entirely. Theater ratio is low (0.10) as the primary function is genuine risk allocation, not performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the affected public, this constraint is a beneficial coordination mechanism that ensures accountability. From the perspective of developers and deployers, it is an extractive mechanism that imposes significant costs and risks. Regulatory bodies view it as a necessary tool for responsible innovation. The engine's per-seat classification will reflect these divergences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers and deployers are the primary payers/targets, bearing the costs of compliance, insurance, and potential liability. The affected public and insurance providers are the beneficiaries, gaining from clearer redress pathways and new market opportunities, respectively. Regulatory bodies act as agenda-setters, defining and enforcing the framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_complexity,
    'How precisely can ''causal contribution'' and ''control'' be defined and measured in complex, emergent AI systems, and what is the practical burden of proof?',
    'Development of industry standards for AI system documentation, explainability tools, and legal precedents from early cases. If attribution remains highly ambiguous, the framework''s effectiveness is compromised.',
    'If attribution is too complex or costly, the framework''s coordination function (risk management, redress) will degrade, and it may become a Snare of arbitrary extraction or a Piton of performative compliance, as parties struggle to meet unclear obligations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_attribution_complexity, empirical, 'Uncertainty regarding the practical feasibility of attributing causality and control in AI systems.').

omega_variable(
    small_developer_impact,
    'Does the distributed liability framework disproportionately burden small AI developers and open-source projects, potentially stifling innovation and market entry?',
    'Empirical studies on the compliance costs and liability exposure of small entities versus large corporations under the framework. Policy adjustments (e.g., safe harbors, tiered liability) could mitigate negative impacts.',
    'If the burden is disproportionate, the constraint''s coordination function for broad innovation will be undermined, shifting it towards a Snare for smaller players while benefiting larger, more resourced entities. This would also increase resistance from the developer community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_developer_impact, empirical, 'Potential for disproportionate impact on small developers and open-source projects.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''shared_liability'' reading genuinely distinct from ''developer_liability'' and ''deployer_liability'', or does it merely represent a compromise that could collapse into one of the other readings under specific legal interpretations?',
    'Analysis of judicial interpretations and regulatory guidance over time. If courts consistently favor one party over the other despite the ''shared'' framework, the reading''s distinctness is undermined.',
    'If the reading collapses, the constraint''s classification would shift to reflect the dominant liability concentration (e.g., more Snare-like if concentrated on a powerless party), and the coordination benefits of distributed risk management would be lost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Ambiguity regarding the stability and distinctness of the ''shared_liability'' reading compared to concentrated liability readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.05).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__shared_liability, theater_ratio, 5, 0.07).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.08).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__shared_liability, theater_ratio, 15, 0.09).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.1).

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
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, ai_safety_standards).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, ai_explainability_requirements).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'liability_attribution' kernel. It is linked to sibling readings 'developer_liability' and 'deployer_liability' as part of a constraint family addressing AI liability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
