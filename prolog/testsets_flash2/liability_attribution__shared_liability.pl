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
 *   systems, where responsibility for harm is distributed among developers
 *   and deployers based on their causal contribution and control. It is one
 *   reading of the broader 'liability_attribution' kernel, which also
 *   includes readings that concentrate liability solely on developers or
 *   deployers. This 'shared_liability' reading aims to address the complexity
 *   of AI systems by ensuring accountability across the value chain,
 *   fostering the emergence of new insurance and indemnification markets, and
 *   distributing the burden of opacity.
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
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '5fddedcc-18af-464a-9b72-135313284ee8').
narrative_ontology:cs_kernel_codification('5fddedcc-18af-464a-9b72-135313284ee8', formalized).
narrative_ontology:cs_authority_grounding('5fddedcc-18af-464a-9b72-135313284ee8', lineage).
narrative_ontology:cs_interpretation_layer_present('5fddedcc-18af-464a-9b72-135313284ee8').
narrative_ontology:cs_reading_relation('5fddedcc-18af-464a-9b72-135313284ee8', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('5fddedcc-18af-464a-9b72-135313284ee8', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('5fddedcc-18af-464a-9b72-135313284ee8', foundational, liability_proportional_to_causal_contribution_and_control).
narrative_ontology:cs_axiom_status(liability_proportional_to_causal_contribution_and_control, holdable).
narrative_ontology:cs_axiom_grounding('5fddedcc-18af-464a-9b72-135313284ee8', liability_proportional_to_causal_contribution_and_control, deontological).
narrative_ontology:cs_axiom('5fddedcc-18af-464a-9b72-135313284ee8', secondary, distributed_accountability_incentivizes_systemic_safety).
narrative_ontology:cs_axiom_status(distributed_accountability_incentivizes_systemic_safety, holdable).
narrative_ontology:cs_axiom_grounding('5fddedcc-18af-464a-9b72-135313284ee8', distributed_accountability_incentivizes_systemic_safety, instrumental).
narrative_ontology:cs_reference_frame('5fddedcc-18af-464a-9b72-135313284ee8', holistic_accountability_framework).
narrative_ontology:cs_drift_state('5fddedcc-18af-464a-9b72-135313284ee8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5fddedcc-18af-464a-9b72-135313284ee8', '').
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

% Responsible for integrating, configuring, and operating AI systems in specific contexts. Bears a portion of liability proportional to their causal contribution and control over the system's deployment environment and operational decisions. Faces increased compliance costs, contractual negotiations, and potential litigation.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_deployers, payer,
    powerful, biographical, constrained, national).

% Receives clearer avenues for redress and compensation when harm occurs due to AI systems, as liability is distributed rather than concentrated on a single party. Benefits from increased accountability across the value chain.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, affected_public, beneficiary,
    powerless, immediate, trapped, local).

% Develops new insurance products and indemnification contracts to cover distributed AI liability. Benefits from new market opportunities and the ability to price risk more accurately across the value chain, but also faces new actuarial challenges.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_providers, beneficiary,
    organized, generational, mobile, global).

% Establishes and enforces the legal framework for shared liability, defining criteria for causal contribution and control. Aims to incentivize responsible AI development and deployment by distributing accountability.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for allocating responsibility and managing risk across the complex AI value chain, ensuring that no single party is solely burdened and that victims have clear recourse. Encourages collaboration on safety standards.
% TRANSFER_FUNCTION: Transfers financial risk and compliance burden from a single point of failure (either developer or deployer) to multiple parties based on their involvement, and ultimately from victims to liable parties and their insurers.
% ABSENT_VOICES: Small AI startups and individual developers, who might argue that the increased compliance and insurance costs disproportionately burden innovation and market entry, potentially leading to market consolidation. Their concerns are often voiced through industry associations but lack direct representation in policy formulation.
% DISAPPEARANCE_RATIONALE: If shared liability vanished, the legal landscape for AI harm would revert to a more fragmented or concentrated model (e.g., solely developer or deployer liability), leading to significant shifts in risk allocation, insurance markets, and potentially reduced victim recourse. The incentives for responsible design and deployment would also change dramatically.
% FOUNDING_PROBLEM: The traditional liability frameworks struggled to attribute fault in complex AI systems, where harm often arises from the interaction of multiple components and contextual factors, leaving victims without clear avenues for redress and creating moral hazard for developers/deployers.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, consumer advocacy groups, and independent AI ethics researchers corroborate that the problem of attributing liability in complex AI systems remains live and pressing, necessitating new legal frameworks. Industry players acknowledge the complexity but often dispute the proposed solutions.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Tangled Rope because it genuinely solves a coordination problem (allocating responsibility in complex AI systems) but also involves asymmetric extraction (developers and deployers bear increased costs, while the affected public and insurance providers benefit). Extractiveness (0.45) reflects the compliance costs, insurance premiums, and potential litigation expenses borne by liable parties. Suppression (0.30) is moderate, as it primarily involves legal enforcement rather than physical coercion, but it does suppress the option of avoiding liability entirely. Theater ratio is low (0.10) as the framework's primary function is genuinely to attribute liability, not merely to perform accountability.
 *
 * PERSPECTIVAL GAP:
 *   Developers and deployers experience this as an extractive constraint due to increased costs and risks, while the affected public and regulators perceive it as a necessary coordination mechanism for accountability. Insurance providers see it as a new market opportunity. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers and deployers are the primary payers/targets, bearing the costs of compliance and potential liability. The affected public is a beneficiary, gaining clearer avenues for redress. Insurance providers are also beneficiaries, as the new liability landscape creates a market for their services. Regulatory bodies act as agenda-setters, defining and enforcing the framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_contribution_measurement_ambiguity,
    'How precisely can ''causal contribution'' and ''control'' be measured and attributed in complex, opaque AI systems?',
    'Development of standardized AI auditing tools, explainable AI (XAI) techniques, and legal precedents that clarify attribution criteria.',
    'If attribution remains highly ambiguous, the constraint''s extractiveness for developers/deployers could become arbitrary or disproportionate, potentially shifting its classification towards a Snare due to unpredictable and unfair burdens. If clear, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_measurement_ambiguity, empirical, 'Uncertainty in measuring causal contribution and control for liability attribution.').

omega_variable(
    insurance_market_efficiency,
    'Will the emerging insurance and indemnification markets for AI liability be efficient and accessible, or will they become another layer of extraction?',
    'Empirical observation of insurance premium trends, market competition, and accessibility for small and medium-sized enterprises (SMEs) over time.',
    'If insurance markets become highly extractive or inaccessible, the overall burden on developers and deployers could increase significantly, pushing the constraint closer to a Snare. If efficient, it supports the coordination function of the Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurance_market_efficiency, empirical, 'Efficiency and accessibility of AI liability insurance markets.').

omega_variable(
    reading_structural_delta,
    'What is the precise structural difference in coordination costs and opacity burden distribution between this ''shared_liability'' reading and the ''developer_liability'' or ''deployer_liability'' readings?',
    'Comparative legal and economic analysis of jurisdictions adopting different liability models, focusing on compliance costs, innovation rates, and victim compensation outcomes.',
    'If the structural delta is negligible, the ''shared_liability'' reading''s claim to superior coordination might be undermined, suggesting it''s merely a re-framing of existing extraction. If substantial, it validates the distinct structural properties of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_structural_delta, conceptual, 'Structural differences between shared, developer, and deployer liability readings.').


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
narrative_ontology:measurement(liab_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.09).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__shared_liability, theater_ratio, 15, 0.1).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(liab_be_t5, liability_attribution__shared_liability, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(liab_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.4).
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

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'liability_attribution' kernel. It is structurally distinct from 'developer_liability' and 'deployer_liability' readings, which concentrate liability differently. All three are part of the same kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
