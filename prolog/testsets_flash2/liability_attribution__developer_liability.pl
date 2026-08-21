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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer Primary Liability for AI/ML Systems
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint describes a legal and regulatory framework where
 *   developers of AI/ML systems bear primary liability for harms caused by
 *   their creations. It is one reading of the broader 'liability_attribution'
 *   kernel, which seeks to assign responsibility in complex AI value chains.
 *   This 'developer_liability' reading places the burden on the creators of
 *   the underlying capability, externalizing risk from deployers and
 *   simplifying regulatory enforcement. The metrics reflect a substantially
 *   extractive and actively enforced constraint, despite being claimed as a
 *   'tangled_rope' (implying some coordination function).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.68).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.75).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Primary Liability for AI/ML Systems").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, 'c4274df9-c4ef-4a09-9f98-1ebf1911b31b').
narrative_ontology:cs_kernel_codification('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', formalized).
narrative_ontology:cs_authority_grounding('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', lineage).
narrative_ontology:cs_interpretation_layer_present('c4274df9-c4ef-4a09-9f98-1ebf1911b31b').
narrative_ontology:cs_reading_relation('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', foundational, creator_bears_primary_responsibility).
narrative_ontology:cs_axiom_status(creator_bears_primary_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', creator_bears_primary_responsibility, deontological).
narrative_ontology:cs_axiom('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', secondary, upstream_control_implies_upstream_risk).
narrative_ontology:cs_axiom_status(upstream_control_implies_upstream_risk, holdable).
narrative_ontology:cs_axiom_grounding('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', upstream_control_implies_upstream_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', traditional_product_liability_framework).
narrative_ontology:cs_drift_state('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', contemporary_ai_complexity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c4274df9-c4ef-4a09-9f98-1ebf1911b31b', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, ai_system_deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, regulatory_bodies).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, ai_system_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary legal and financial burden for harms caused by AI systems they create, even when deployed in contexts they do not control. This includes costs for design flaws, data biases, and unforeseen emergent behaviors. Exit options are limited by the need to operate in the market and the high cost of compliance or litigation.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, ai_system_developers, payer,
    powerful, biographical, constrained, global).

% Benefit from externalized risk, as primary liability rests with developers. They can deploy AI systems with less direct legal exposure, focusing on operational integration rather than deep technical liability assessment. Their exit options include choosing different developers or lobbying for more favorable liability regimes.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, ai_system_deployers, beneficiary,
    institutional, generational, mobile, national).

% Establish and enforce the liability framework, finding it simpler to attribute primary responsibility to the 'creator' of the underlying capability. This simplifies regulatory oversight and enforcement actions, reducing the complexity of investigating deployment-specific factors. They can adjust regulations or pursue enforcement actions.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Indirectly bear costs through reduced innovation or higher prices if developers become overly cautious. They benefit from the theoretical accountability this framework provides, but often face practical barriers to seeking redress. Their exit options are limited to avoiding specific AI systems or engaging in collective action.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, end_users, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(liability_attribution__developer_liability, end_users, beneficiary).

% Analyze the implications of developer-centric liability, debating its fairness, effectiveness, and impact on innovation. They provide critical commentary and propose alternative frameworks, but do not directly participate in the enforcement or payment of the constraint.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ascribes clear responsibility for AI system harms, aiming to incentivize developers to build safer, more robust, and transparent systems from the outset. It simplifies legal processes by focusing on a single point of accountability.
% TRANSFER_FUNCTION: Transfers the primary burden of risk and legal defense costs from deployers and the public to AI system developers. It also transfers the burden of managing system opacity and potential harms to the developer.
% ABSENT_VOICES: Small and independent developers, who would argue that this framework disproportionately burdens them compared to large corporations, potentially stifling innovation and market entry. They are often excluded from the regulatory design process due to lack of resources and lobbying power.
% DISAPPEARANCE_RATIONALE: If developer primary liability vanished overnight, there would be a significant shift in risk allocation. Deployers would face increased liability, potentially leading to more cautious AI adoption or a push for new liability frameworks. Developers might innovate more freely but also with less direct accountability, leading to a period of legal uncertainty and re-negotiation of contracts.
% FOUNDING_PROBLEM: The rapid proliferation of complex AI systems created a 'liability gap' where it was unclear who was responsible for harms, leading to a lack of accountability and potential for unaddressed societal risks.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and some public interest groups attest that the problem of AI liability remains live, citing ongoing incidents of AI-related harm. Deployers and developers, however, contest whether this specific attribution scheme is the most effective solution, often pointing to the complexity of AI value chains.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.68) because developers bear significant costs for harms they may not directly control, and this cost is passed through the value chain. Suppression (0.75) is also high, as developers face strong legal and market pressure to comply, with limited options to shift liability. The theater ratio (0.20) is moderate; while there's genuine effort to ensure accountability, some of the enforcement activity serves to maintain the simplified liability structure rather than fully address the complexity of AI harms. The constraint is claimed as a 'tangled_rope' because it attempts to coordinate accountability and safety incentives (benefiting deployers and regulators) while simultaneously extracting from developers.
 *
 * PERSPECTIVAL GAP:
 *   Developers experience this as a highly extractive and suppressive constraint, forcing them to internalize risks beyond their direct control. Deployers, in contrast, perceive it as a beneficial coordination mechanism that clarifies responsibility and reduces their own exposure. Regulatory bodies view it as an effective, albeit imperfect, tool for governance. The engine's classification will highlight this divergence between the claimed coordination and the experienced extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   AI system developers are the primary targets (payers) of this constraint, bearing the costs of liability. AI system deployers are beneficiaries, as their direct liability is reduced. Regulatory bodies are agenda-setters and beneficiaries, as the framework simplifies their enforcement task. End users are indirect payers (through potential innovation stifling or higher prices) but also indirect beneficiaries of the accountability. Legal scholars act as observers, analyzing the system without direct participation in its costs or benefits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_chain_opacity,
    'How accurately can primary causation be attributed to the developer''s capability versus the deployer''s context-specific use or data inputs?',
    'Development of robust, standardized AI auditing tools and methodologies that can trace causal pathways of harm through complex AI systems and their deployment environments.',
    'If causation is frequently found to be context-dependent, this developer-centric liability model would be revealed as structurally misaligned, potentially leading to reclassification towards ''deployer_liability'' or ''shared_liability'' readings. If developer-side flaws are consistently primary, it reinforces this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_chain_opacity, empirical, 'Ambiguity in attributing harm to developer-side vs. deployer-side factors.').

omega_variable(
    innovation_stifling_vs_safety,
    'Does primary developer liability genuinely incentivize safer AI development, or does it primarily stifle innovation, especially for smaller entities?',
    'Longitudinal studies tracking AI innovation rates, market entry of new developers, and documented safety improvements under this liability regime, compared to alternative regimes or counterfactuals.',
    'If innovation is significantly stifled without a commensurate increase in safety, the coordination function of this constraint would be undermined, pushing its classification closer to a ''snare''. If safety demonstrably improves, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_stifling_vs_safety, empirical, 'Trade-off between safety incentives and innovation impact.').

omega_variable(
    reading_framing_legitimacy,
    'Is the ''developer_liability'' reading a legitimate and coherent interpretation of liability attribution, or is it a simplification driven by regulatory convenience?',
    'Conceptual analysis and legal philosophical debate on the principles of responsibility in distributed systems, assessing whether ''creator'' status is a sufficient basis for primary liability in AI contexts. This would involve comparing its internal consistency with other liability theories.',
    'If found to be primarily a simplification, its legitimacy as a ''tangled_rope'' (implying a genuine coordination function) would be weakened, pushing it towards a ''snare'' classification. If its conceptual grounding is robust, it strengthens the current classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Conceptual coherence of developer-centric liability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__developer_liability, theater_ratio, 5, 0.13).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__developer_liability, theater_ratio, 10, 0.16).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__developer_liability, theater_ratio, 15, 0.18).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__developer_liability, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(liab_be_t5, liability_attribution__developer_liability, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(liab_be_t10, liability_attribution__developer_liability, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(liab_be_t15, liability_attribution__developer_liability, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(liab_be_t20, liability_attribution__developer_liability, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(liab_su_t5, liability_attribution__developer_liability, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(liab_su_t10, liability_attribution__developer_liability, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(liab_su_t15, liability_attribution__developer_liability, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(liab_su_t20, liability_attribution__developer_liability, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, ai_ethics_guidelines).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, ai_safety_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'liability_attribution' kernel, alongside 'deployer_liability' and 'shared_liability'. Each reading offers a distinct structural assignment of responsibility for AI harms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
