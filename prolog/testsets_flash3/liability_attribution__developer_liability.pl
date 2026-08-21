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
 *   This constraint story describes the 'developer_liability' reading of the
 *   broader 'liability_attribution' kernel in AI governance. Under this
 *   reading, the primary legal and financial responsibility for harms caused
 *   by AI systems is attributed to the developers who create the underlying
 *   capability. This framework is often advocated by regulatory bodies and
 *   deployers seeking to externalize risk, but it places a significant burden
 *   on developers, potentially stifling innovation. The constraint is claimed
 *   as a 'tangled_rope' because it attempts to coordinate accountability
 *   while simultaneously extracting significant costs from developers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.65).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.7).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.65).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Primary Liability for AI/ML Systems").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, 'fa2251a1-72ef-480b-954e-13953b7babeb').
narrative_ontology:cs_kernel_codification('fa2251a1-72ef-480b-954e-13953b7babeb', formalized).
narrative_ontology:cs_authority_grounding('fa2251a1-72ef-480b-954e-13953b7babeb', lineage).
narrative_ontology:cs_interpretation_layer_present('fa2251a1-72ef-480b-954e-13953b7babeb').
narrative_ontology:cs_reading_relation('fa2251a1-72ef-480b-954e-13953b7babeb', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('fa2251a1-72ef-480b-954e-13953b7babeb', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('fa2251a1-72ef-480b-954e-13953b7babeb', foundational, creator_bears_ultimate_responsibility).
narrative_ontology:cs_axiom_status(creator_bears_ultimate_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('fa2251a1-72ef-480b-954e-13953b7babeb', creator_bears_ultimate_responsibility, deontological).
narrative_ontology:cs_axiom('fa2251a1-72ef-480b-954e-13953b7babeb', foundational, upstream_control_implies_liability).
narrative_ontology:cs_axiom_status(upstream_control_implies_liability, holdable).
narrative_ontology:cs_axiom_grounding('fa2251a1-72ef-480b-954e-13953b7babeb', upstream_control_implies_liability, empirically_contingent).
narrative_ontology:cs_reference_frame('fa2251a1-72ef-480b-954e-13953b7babeb', traditional_product_liability_framework).
narrative_ontology:cs_drift_state('fa2251a1-72ef-480b-954e-13953b7babeb', contemporary_ai_governance_debate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fa2251a1-72ef-480b-954e-13953b7babeb', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, ai_deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, regulatory_bodies).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, ai_developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, innovation_ecosystem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, affected_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary legal and financial burden for harms caused by AI systems they create, even when those systems are deployed in contexts beyond their direct control. This includes costs for legal defense, settlements, and increased insurance premiums. Exit means abandoning the field or moving to less regulated jurisdictions.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, ai_developers, payer,
    powerful, biographical, constrained, global).

% Benefit from externalized risk, as primary liability shifts to developers. They can deploy AI systems with reduced direct legal exposure, focusing on operational benefits. Their situation is improved by this reading, as they face less pressure to understand or mitigate upstream risks.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, ai_deployers, beneficiary,
    institutional, biographical, mobile, national).

% Enforce the liability framework, seeking to hold developers accountable for system harms. They benefit from a clear, identifiable party to target for enforcement, simplifying regulatory oversight. Their goal is to ensure public safety and trust in AI, but this reading simplifies their enforcement task.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Potentially benefit from having a clear party (the developer) to seek redress from in case of harm. However, the complexity of AI systems often makes proving causation difficult, and the benefit is contingent on successful litigation.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, affected_public, beneficiary,
    powerless, immediate, trapped, local).

% Suffers from increased compliance costs, reduced investment in high-risk but potentially transformative AI research, and a chilling effect on new ventures. The burden of liability can stifle innovation, especially for smaller entities. Exit means slower growth or relocation.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, innovation_ecosystem, payer,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate accountability for AI harms by assigning a clear primary party responsible for system safety and ethical design, thereby incentivizing responsible development practices.
% TRANSFER_FUNCTION: Transfers legal and financial risk, as well as the burden of understanding and mitigating systemic opacity, from AI deployers and the general public to AI developers.
% ABSENT_VOICES: Smaller AI startups and academic researchers, who would argue that this liability model disproportionately burdens innovation and fails to account for the distributed nature of AI risk, are often underrepresented in policy discussions.
% DISAPPEARANCE_RATIONALE: If developer primary liability vanished, the regulatory landscape for AI would immediately shift, with deployers facing increased scrutiny and pressure to internalize risk. Developers might innovate more freely but also with less direct accountability, leading to a scramble for new liability frameworks.
% FOUNDING_PROBLEM: The rapid proliferation of AI systems created a 'liability gap' where it was unclear who was responsible for harms caused by autonomous or opaque systems, leading to a lack of accountability and potential public distrust.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and some consumer advocacy groups attest that the problem of AI accountability remains live. AI developers and some legal scholars, however, argue that this specific attribution model is an oversimplification that creates new problems rather than solving the original one effectively.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(liability_attribution__developer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__developer_liability, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the substantial legal costs, insurance burdens, and potential damages developers face. Suppression (0.70) is high due to the regulatory and tort mechanisms that compel developers to accept this liability, with limited options to shift or share the risk. The theater ratio (0.20) is moderate; while there's genuine intent to ensure accountability, some of the enforcement activity serves to maintain the current risk externalization rather than purely improving safety. Accessibility collapse (0.40) is moderate as developers can still operate, but with significantly higher barriers. Resistance (0.55) is present from developer communities and some legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of AI deployers and regulators, this framework provides a clear, efficient mechanism for accountability (a 'rope' or 'scaffold'). From the perspective of AI developers and the broader innovation ecosystem, it is a highly extractive and suppressive 'snare' or 'tangled_rope' that misattributes risk and stifles progress. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers are clear payers/victims, bearing the brunt of the liability. AI deployers are beneficiaries, as their direct liability is reduced. Regulatory bodies act as agenda-setters, enforcing this attribution model. The affected public are indirect beneficiaries, gaining a clearer path to redress, though often a difficult one. The innovation ecosystem is a payer, experiencing a chilling effect.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_complexity,
    'How accurately can primary causation for AI harms be attributed solely to developers, given the complexity of deployment contexts, user interaction, and emergent system behavior?',
    'Development of robust, standardized AI auditing tools and methodologies that can trace causal chains through complex AI systems and their operational environments.',
    'If attribution to developers is consistently inaccurate or incomplete, the constraint''s legitimacy as a coordination mechanism for accountability would collapse, pushing it towards a ''snare'' or ''piton'' for developers. If attribution is robust, it strengthens the ''tangled_rope'' or even ''rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_attribution_complexity, empirical, 'The empirical challenge of isolating developer-specific causation in complex AI systems.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, regulatory enforcement) or internalized (developers self-censor innovation due to fear of liability)?',
    'Post-policy-change innovation trajectory: if innovation remains stifled after structural liability is reduced, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — developers carry the suppression with them after policy changes, impacting the innovation ecosystem more severely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for AI developers.').

omega_variable(
    reading_framing_legitimacy,
    'Is the ''developer_liability'' reading a legitimate and effective approach to AI accountability, or is it a convenient externalization of risk by deployers and regulators?',
    'Comparative analysis of AI safety and innovation outcomes in jurisdictions adopting different liability models (deployer-centric, shared, or developer-centric).',
    'If other models prove more effective at promoting safety and innovation, this reading''s legitimacy would erode, shifting its classification towards a ''snare'' or ''piton'' from an ''observer'' seat. If it proves uniquely effective, its ''tangled_rope'' classification might lean more towards a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'The conceptual validity of attributing primary liability to developers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__developer_liability, theater_ratio, 5, 0.18).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__developer_liability, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(liab_be_t5, liability_attribution__developer_liability, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(liab_be_t10, liability_attribution__developer_liability, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(liab_su_t5, liability_attribution__developer_liability, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(liab_su_t10, liability_attribution__developer_liability, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, ai_ethics_guidelines).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, ai_safety_standards).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('developer_liability') of the 'liability_attribution' kernel. Other readings include 'deployer_liability' and 'shared_liability', which attribute primary responsibility differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
