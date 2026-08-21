% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__developer_liability, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Primary Developer Liability for Capability Harms
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint describes the legal and regulatory framework where
 *   developers of underlying technological capabilities (e.g., AI models,
 *   software components) bear primary liability for harms, regardless of the
 *   specific deployment context. This is one reading of the broader
 *   'liability_attribution' kernel, specifically the 'developer_liability'
 *   reading. It contrasts with alternative framings that place liability
 *   primarily on deployers or distribute it more broadly. The framework is
 *   actively enforced through legal and regulatory mechanisms, imposing
 *   significant costs and risks on developers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.82).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.78).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.82).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, snare).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Primary Developer Liability for Capability Harms").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, '62cae551-b88b-4d89-9bf1-96666e1ffeb1').
narrative_ontology:cs_kernel_codification('62cae551-b88b-4d89-9bf1-96666e1ffeb1', formalized).
narrative_ontology:cs_authority_grounding('62cae551-b88b-4d89-9bf1-96666e1ffeb1', lineage).
narrative_ontology:cs_interpretation_layer_present('62cae551-b88b-4d89-9bf1-96666e1ffeb1').
narrative_ontology:cs_reading_relation('62cae551-b88b-4d89-9bf1-96666e1ffeb1', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('62cae551-b88b-4d89-9bf1-96666e1ffeb1', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('62cae551-b88b-4d89-9bf1-96666e1ffeb1', foundational, creator_responsibility_principle).
narrative_ontology:cs_axiom_status(creator_responsibility_principle, holdable).
narrative_ontology:cs_axiom_grounding('62cae551-b88b-4d89-9bf1-96666e1ffeb1', creator_responsibility_principle, deontological).
narrative_ontology:cs_axiom('62cae551-b88b-4d89-9bf1-96666e1ffeb1', foundational, upstream_control_implies_liability).
narrative_ontology:cs_axiom_status(upstream_control_implies_liability, holdable).
narrative_ontology:cs_axiom_grounding('62cae551-b88b-4d89-9bf1-96666e1ffeb1', upstream_control_implies_liability, empirically_contingent).
narrative_ontology:cs_reference_frame('62cae551-b88b-4d89-9bf1-96666e1ffeb1', traditional_product_liability_framework).
narrative_ontology:cs_drift_state('62cae551-b88b-4d89-9bf1-96666e1ffeb1', ai_era_complexity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62cae551-b88b-4d89-9bf1-96666e1ffeb1', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_users_public).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, developers).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, creator_responsibility_principle).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, upstream_control_implies_liability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As creators of underlying capabilities (e.g., AI models, software libraries), they bear the primary legal and financial burden for harms caused by their creations, even when deployed in contexts they do not control. This includes costs for compliance, insurance, and potential litigation. Exit means abandoning the market or shifting to less impactful, less risky development.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, developers, payer,
    organized, biographical, constrained, global).

% Organizations or individuals who integrate and use the capabilities created by developers. They benefit from externalizing much of the liability risk to developers, simplifying their own compliance and operational risk management. They retain flexibility in choosing which capabilities to deploy and how.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deployers, beneficiary,
    institutional, generational, mobile, global).

% Government bodies responsible for creating and enforcing liability frameworks. They find it simpler to attribute primary liability to developers due to perceived 'upstream' control and the difficulty of tracing complex causal chains in deployment. This simplifies enforcement and provides a clear target for accountability.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulators_legislators, agenda_setter,
    institutional, generational, analytical, national).

% Individuals or groups affected by the deployment of technological capabilities. They benefit from a clear party being held accountable for harms, theoretically leading to safer products and recourse. However, they bear indirect costs if developers pass on liability burdens or if innovation is stifled.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, end_users_public, beneficiary,
    powerless, immediate, constrained, global).

% Analyze the implications of liability frameworks, often highlighting the complexities of causal attribution in AI systems and the potential for unintended consequences on innovation or fairness. They do not directly benefit or pay but influence the discourse around the constraint.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, legal_scholars_ethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, deployers).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ascribes clear responsibility for harms arising from complex technological capabilities, providing a legal and financial target for redress and incentivizing safety measures at the creation stage.
% TRANSFER_FUNCTION: Transfers the primary burden of risk, compliance costs, and potential financial penalties for technological harms from deployers and end-users to developers.
% ABSENT_VOICES: Smaller independent developers and open-source communities, who would argue that such broad liability stifles innovation, disproportionately impacts those with fewer resources, and misattributes control. They are often excluded from the legislative drafting process due to lack of lobbying power.
% DISAPPEARANCE_RATIONALE: If primary developer liability vanished overnight, the legal and economic landscape for technology would immediately shift. Deployers would face significantly increased risk, potentially leading to a slowdown in adoption or a scramble for new insurance models. Developers might innovate more freely but also with less incentive for upstream safety. The entire risk allocation model for AI and advanced software would need to be re-engineered.
% FOUNDING_PROBLEM: The increasing complexity and opacity of advanced technological systems (e.g., AI, autonomous systems) made it difficult to attribute responsibility for harms, leading to a perceived accountability gap and a lack of clear incentives for safety.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and some consumer advocacy groups attest that the problem of accountability for complex tech harms is still live. Legal scholars and industry bodies (outside the direct beneficiaries) corroborate the existence of the problem but often contest whether developer-centric liability is the most effective or equitable solution.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.82, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__developer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__developer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__developer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because developers incur substantial costs for compliance, insurance, and potential legal defense, effectively subsidizing the risk profiles of deployers. Suppression is also high (0.78) as developers are legally compelled to adhere to these frameworks, with limited options to avoid liability while remaining active in the market. The theater ratio is low (0.15) because the liability is genuinely imposed and enforced, not merely performative. Accessibility collapse is high (0.70) as alternatives to operating under this liability regime (e.g., open-source without liability, or operating in jurisdictions with different rules) are significantly constrained by market pressures and global regulatory trends. Resistance is moderate (0.45) as developers and their associations lobby for changes but largely comply.
 *
 * PERSPECTIVAL GAP:
 *   Developers experience this as a snare, where the coordination story (clear accountability) serves as a cover for externalizing risk from deployers. Deployers, conversely, experience it as a beneficial coordination mechanism that clarifies their own risk landscape. Regulators view it as an efficient mechanism for accountability in complex systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers are the primary targets (victims) as they bear the direct costs and risks. Deployers are beneficiaries, as their risk is externalized. End-users are also beneficiaries, as the framework aims to provide accountability for harms, though they may bear indirect costs. Regulators are agenda-setters, shaping the framework to achieve policy goals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a distinct reading of the ''liability_attribution'' kernel, or merely a policy preference within a broader ''shared_liability'' framework?',
    'Analysis of legal precedents and legislative intent: if courts consistently prioritize developer liability even when deployer control is evident, it supports a distinct reading.',
    'If it''s a distinct reading, the classification holds. If it''s merely a policy choice within ''shared_liability'', the ''shared_liability'' constraint would need to be re-evaluated for its true extractiveness and suppression across different actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the distinct identity of this kernel reading.').

omega_variable(
    causal_control_vs_attribution,
    'Does primary developer liability accurately reflect actual causal control over harms, especially in complex AI systems where deployment context is critical?',
    'Empirical studies of AI system failures and harm attribution, tracing causal chains from development to deployment and impact.',
    'If causal control is often distributed or primarily at the deployment stage, the ''developer_liability'' framework''s justification weakens, potentially increasing its measured extractiveness and suppression as it misattributes responsibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_control_vs_attribution, empirical, 'Ambiguity between legal attribution and actual causal control.').

omega_variable(
    innovation_impact,
    'To what extent does primary developer liability stifle innovation, particularly for smaller developers and open-source projects, by imposing prohibitive risk and compliance costs?',
    'Economic studies tracking innovation rates, startup formation, and open-source contributions in jurisdictions with strong developer liability versus those with alternative frameworks.',
    'If innovation is significantly stifled, the constraint''s overall societal cost (beyond direct extraction) is higher, potentially leading to a re-evaluation of its policy efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_impact, empirical, 'Impact of liability on technological innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.18).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__developer_liability, theater_ratio, 6, 0.17).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__developer_liability, theater_ratio, 12, 0.16).
narrative_ontology:measurement(liab_tr_t18, liability_attribution__developer_liability, theater_ratio, 18, 0.15).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__developer_liability, theater_ratio, 24, 0.15).
narrative_ontology:measurement(liab_tr_t30, liability_attribution__developer_liability, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(liab_be_t6, liability_attribution__developer_liability, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(liab_be_t12, liability_attribution__developer_liability, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(liab_be_t18, liability_attribution__developer_liability, base_extractiveness, 18, 0.79).
narrative_ontology:measurement(liab_be_t24, liability_attribution__developer_liability, base_extractiveness, 24, 0.81).
narrative_ontology:measurement(liab_be_t30, liability_attribution__developer_liability, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(liab_su_t6, liability_attribution__developer_liability, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(liab_su_t12, liability_attribution__developer_liability, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(liab_su_t18, liability_attribution__developer_liability, suppression_requirement, 18, 0.74).
narrative_ontology:measurement(liab_su_t24, liability_attribution__developer_liability, suppression_requirement, 24, 0.77).
narrative_ontology:measurement(liab_su_t30, liability_attribution__developer_liability, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'liability_attribution' kernel, each representing a distinct structural claim about where primary responsibility lies for harms from complex technologies. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
