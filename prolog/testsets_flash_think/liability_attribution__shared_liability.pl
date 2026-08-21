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
 *   human_readable: Shared Liability for Technology Value Chains
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint describes the legal and regulatory framework for 'shared
 *   liability' in technology value chains, where responsibility for harms is
 *   distributed among developers and deployers based on their causal
 *   contribution and control. It is one reading of the broader
 *   'liability_attribution' kernel, which also includes 'developer_liability'
 *   and 'deployer_liability' as sibling readings. This reading aims to
 *   address accountability gaps in complex systems by ensuring multiple
 *   parties bear responsibility, fostering a more robust risk management
 *   ecosystem.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.65).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.7).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.65).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Shared Liability for Technology Value Chains").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e').
narrative_ontology:cs_kernel_codification('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', formalized).
narrative_ontology:cs_authority_grounding('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', lineage).
narrative_ontology:cs_interpretation_layer_present('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e').
narrative_ontology:cs_reading_relation('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', foundational, causal_contribution_proportionality).
narrative_ontology:cs_axiom_status(causal_contribution_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', causal_contribution_proportionality, empirically_contingent).
narrative_ontology:cs_axiom('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', foundational, control_as_liability_basis).
narrative_ontology:cs_axiom_status(control_as_liability_basis, holdable).
narrative_ontology:cs_axiom_grounding('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', control_as_liability_basis, empirically_contingent).
narrative_ontology:cs_reference_frame('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', holistic_risk_distribution).
narrative_ontology:cs_drift_state('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1147c1c7-e6fc-4eb7-9450-4ebe4f2f894e', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, end_users).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_providers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As creators of technology, they bear a portion of liability proportional to their causal contribution and control. This increases their compliance costs, legal review, and insurance premiums, but also provides a clearer framework for risk management than full, sole liability.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, developers, payer,
    organized, biographical, constrained, global).

% As operators of technology in specific contexts, they bear a portion of liability proportional to their causal contribution and control. This necessitates investment in risk assessment, operational controls, and insurance, distributing the burden rather than concentrating it solely on them.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, deployers, payer,
    organized, biographical, constrained, global).

% Benefit from increased accountability and clearer avenues for recourse when harms occur due to complex technology. They are less likely to be left without a liable party, improving trust in technology adoption.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, end_users, beneficiary,
    moderate, immediate, mobile, local).

% Benefit from the emergence of new markets for liability insurance and indemnification products tailored to distributed risk. They actively shape the terms of these new markets and influence risk management practices across the value chain.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_providers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, insurance_providers, agenda_setter).

% Define, interpret, and enforce the legal principles of shared liability. They coordinate the distribution of responsibility and adjudicate disputes, ensuring the framework's application aligns with legal theory and public policy goals.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulators_courts, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the theoretical underpinnings and practical implications of shared liability, contributing to its evolution and critique. They provide the analytical lens through which the constraint's effectiveness and fairness are assessed.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, insurance_providers).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure comprehensive accountability and risk distribution across complex technology value chains by attributing liability based on causal contribution and control, preventing gaps where no single party is fully responsible.
% TRANSFER_FUNCTION: Transfers the burden of potential harm and associated legal/insurance costs from end-users to developers and deployers, and then further distributes these costs among developers, deployers, and insurance markets.
% ABSENT_VOICES: Future innovators or small startups might find the increased compliance and insurance costs prohibitive, potentially stifling innovation. Their voices are often not fully represented in the initial design of such liability regimes.
% DISAPPEARANCE_RATIONALE: If shared liability vanished, the technology industry would face significant uncertainty regarding risk, potentially leading to a return to single-point liability models or widespread gaps in accountability, causing major disruption to legal and insurance markets.
% FOUNDING_PROBLEM: The increasing complexity and distributed nature of modern technology systems (e.g., AI, IoT) made it difficult to attribute harm to a single party, leaving victims without clear recourse and creating accountability gaps.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, consumer advocacy groups, and victims of technology-related harms consistently attest to the ongoing challenge of attributing liability in complex systems. Regulatory bodies are actively developing frameworks to address this, corroborating the problem's live status.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates risk distribution and accountability (benefiting end-users and creating new markets for insurance) while simultaneously extracting costs from developers and deployers through increased compliance, legal, and insurance burdens. Extraction (0.65) is moderate-high due to these new costs. Suppression (0.70) is significant as it requires active legal and regulatory enforcement to compel adherence and prevent unilateral attempts to offload liability. Theater ratio (0.15) is low, as the framework is primarily functional, though some performative compliance may exist.
 *
 * PERSPECTIVAL GAP:
 *   Developers and deployers experience this as an extractive burden, increasing their operational costs and legal exposure. End-users and insurance providers, however, perceive it as a beneficial coordination mechanism that enhances safety, accountability, and creates new market opportunities. Regulators view it as a necessary tool for effective governance in complex technological domains.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and deployers are the primary targets (payers) as they bear the direct costs of compliance and insurance. End-users are beneficiaries due to improved recourse and safety. Insurance providers are also beneficiaries, profiting from the new risk markets. Regulators and courts act as agenda-setters, defining and enforcing the terms of this shared liability.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Snare, acknowledging its genuine coordination function in distributing risk and ensuring accountability. It also avoids mislabeling it as a pure Rope, recognizing the significant extraction imposed on developers and deployers. The 'live' status of the founding problem (accountability gaps in complex tech) suggests it has not yet suffered mandatrophy, though its implementation and costs are subject to ongoing debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''shared_liability'' reading of the ''liability_attribution'' kernel?',
    'Analysis of legal texts, regulatory guidance, and judicial precedents to confirm the explicit or implicit adoption of principles of causal contribution and control for liability distribution.',
    'If misidentified, the analysis of its structural relationships to sibling readings (developer_liability, deployer_liability) would be flawed, leading to incorrect network and CS structure interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading of the liability attribution kernel.').

omega_variable(
    structural_delta_validation,
    'Are the expected structural deltas for this reading (developers/deployers in victim set, increased coordination costs, emergence of insurance markets, distributed opacity burden) empirically observable?',
    'Empirical studies on the economic impact of shared liability regimes, analysis of insurance market growth in relevant sectors, and legal case studies on liability allocation.',
    'If these deltas are not observed, the ''shared_liability'' reading''s practical instantiation may be weaker or different than theorized, potentially shifting its effective extractiveness or coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_validation, empirical, 'Verifies the real-world manifestation of the reading''s predicted structural changes.').

omega_variable(
    coordination_cost_justification,
    'Are the increased coordination costs (e.g., legal review, contractual overhead) genuinely necessary for effective risk distribution, or do they contain an element of rent-seeking by legal/insurance industries?',
    'Comparative analysis of transaction costs in different liability regimes and independent audits of legal and insurance service pricing.',
    'If a significant portion of coordination costs is found to be rent-seeking, the effective extractiveness of the constraint would be higher than currently estimated, pushing it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_justification, empirical, 'Assesses the necessity vs. extractive nature of coordination overhead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__shared_liability, theater_ratio, 5, 0.12).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.14).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__shared_liability, theater_ratio, 15, 0.15).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(liab_be_t5, liability_attribution__shared_liability, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(liab_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(liab_be_t15, liability_attribution__shared_liability, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(liab_su_t5, liability_attribution__shared_liability, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(liab_su_t10, liability_attribution__shared_liability, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(liab_su_t15, liability_attribution__shared_liability, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
