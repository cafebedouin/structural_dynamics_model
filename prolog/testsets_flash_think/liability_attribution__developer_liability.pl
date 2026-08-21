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
 *   This constraint represents the legal and regulatory principle that
 *   developers bear primary liability for harms arising from the underlying
 *   capabilities they create. It is a specific reading of the broader
 *   'liability_attribution' kernel, emphasizing the creator's responsibility.
 *   While framed as a coordination mechanism to ensure safety and
 *   accountability, its operation involves substantial extraction from
 *   developers and active suppression of alternative liability models.
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
narrative_ontology:constraint_metric(liability_attribution__developer_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Primary Developer Liability for Capability Harms").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, 'b638352b-857f-4da3-8f72-0cc89889ee58').
narrative_ontology:cs_kernel_codification('b638352b-857f-4da3-8f72-0cc89889ee58', formalized).
narrative_ontology:cs_authority_grounding('b638352b-857f-4da3-8f72-0cc89889ee58', lineage).
narrative_ontology:cs_interpretation_layer_present('b638352b-857f-4da3-8f72-0cc89889ee58').
narrative_ontology:cs_reading_relation('b638352b-857f-4da3-8f72-0cc89889ee58', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('b638352b-857f-4da3-8f72-0cc89889ee58', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('b638352b-857f-4da3-8f72-0cc89889ee58', foundational, creator_responsibility_principle).
narrative_ontology:cs_axiom_status(creator_responsibility_principle, holdable).
narrative_ontology:cs_axiom_grounding('b638352b-857f-4da3-8f72-0cc89889ee58', creator_responsibility_principle, conventional).
narrative_ontology:cs_axiom('b638352b-857f-4da3-8f72-0cc89889ee58', secondary, control_over_source_code).
narrative_ontology:cs_axiom_status(control_over_source_code, holdable).
narrative_ontology:cs_axiom_grounding('b638352b-857f-4da3-8f72-0cc89889ee58', control_over_source_code, empirically_contingent).
narrative_ontology:cs_reference_frame('b638352b-857f-4da3-8f72-0cc89889ee58', traditional_product_liability_framework).
narrative_ontology:cs_drift_state('b638352b-857f-4da3-8f72-0cc89889ee58', contemporary_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b638352b-857f-4da3-8f72-0cc89889ee58', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_users_public).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, insurers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, developers).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, open_source_contributors).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, creator_responsibility_principle).
narrative_ontology:constraint_vindicates(liability_attribution__developer_liability, causal_proximity_to_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary legal and financial risk for harms caused by their software or AI models, even when those are deployed in contexts they did not foresee or control. This includes costs for legal defense, settlements, and increased insurance premiums. Their exit options are limited by the need to participate in the market.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, developers, payer,
    organized, biographical, constrained, global).

% Benefit from reduced direct liability for harms, as primary responsibility is shifted upstream to the creators of the underlying technology. They can more freely integrate and deploy new capabilities with lower direct legal exposure, though they still face reputational risks.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, deployers, beneficiary,
    institutional, generational, mobile, global).

% Design, implement, and enforce the legal and regulatory frameworks that attribute primary liability to developers. Their goal is to incentivize responsible development and provide clear avenues for redress, often responding to public pressure or perceived market failures.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, regulators_legislators, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from clearer accountability and potential avenues for redress in cases of harm caused by technology. However, they may indirectly bear costs through higher prices for software or reduced innovation if developers become overly risk-averse. Their choices are often limited to available technologies.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, end_users_public, beneficiary,
    organized, biographical, constrained, global).

% Profit from the increased demand for liability insurance products among developers. They assess and price the risks associated with developer liability, creating a new market for risk transfer.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, insurers, beneficiary,
    organized, biographical, mobile, global).

% Analyze the implications of attributing primary liability to developers, critiquing its fairness, effectiveness, and impact on innovation. They propose alternative liability frameworks and contribute to public discourse on technology governance.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, legal_scholars_ethicists, observer,
    analytical, generational, analytical, global).

% Face potential liability for contributions to open-source projects, which are often used in commercial applications without their direct knowledge or control. This creates a chilling effect on participation, as the costs of compliance or potential litigation are prohibitive for individuals. Their identity is often tied to the open-source community.
narrative_ontology:constraint_stakeholder(liability_attribution__developer_liability, open_source_contributors, payer,
    powerless, immediate, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__developer_liability, deployers).
narrative_ontology:fixing_cost_class(liability_attribution__developer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear point of accountability for harms arising from technological capabilities, aiming to incentivize careful design and development practices across the industry.
% TRANSFER_FUNCTION: Transfers legal and financial risk, as well as the burden of managing potential harms, from technology deployers and the broader public to the developers of the underlying capabilities.
% ABSENT_VOICES: Smaller independent developers and open-source contributors are often underrepresented in the policy-making process; they would argue that primary liability stifles innovation, disproportionately impacts those with fewer resources, and is impractical for complex, modular software ecosystems.
% DISAPPEARANCE_RATIONALE: If primary developer liability vanished overnight, the allocation of risk and responsibility in the technology sector would fundamentally shift. Deployers would face increased direct liability, potentially leading to more cautious adoption of new technologies. Developers might innovate more freely but with less direct incentive for safety, and the legal landscape for redress would become significantly more ambiguous.
% FOUNDING_PROBLEM: The increasing complexity and societal impact of technology created a need for clear accountability when harms occurred, particularly when the direct cause was embedded within the underlying capability rather than its specific deployment.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and end-user advocacy groups attest that the problem of accountability for tech harms remains live, citing ongoing incidents and the rapid pace of technological change. Legal scholars and some industry groups, while acknowledging the problem, contest whether developer-centric liability is the most effective or equitable solution, suggesting the founding problem is evolving beyond the current framework's capacity.
narrative_ontology:disappearance_verdict(liability_attribution__developer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__developer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__developer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.68) because developers bear significant financial and legal burdens, often disproportionate to their control over deployment contexts. Suppression is also high (0.75) due to the coercive nature of legal frameworks and the limited exit options for developers who wish to participate in the market. Theater ratio is low (0.20) as the enforcement of liability is generally genuine, though some aspects might be performative in complex cases. Accessibility collapse is moderate (0.45) as alternatives (like shared liability) are discussed but not widely adopted. Resistance is moderate (0.55) from developer communities and legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   Developers experience this as a highly extractive and suppressive constraint, limiting innovation and imposing undue risk. Deployers, conversely, perceive it as a beneficial coordination mechanism that externalizes risk and clarifies their own responsibilities. Regulators view it as a necessary tool for public protection and market order.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers are clear targets (high d) as they bear the costs. Deployers, end-users, and insurers are beneficiaries (low d) as they either offload risk, gain clearer redress, or profit from the new risk market. Regulators are agenda-setters, shaping the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring accountability for tech harms) is still live, but its effectiveness is contested in the face of increasingly complex, modular, and AI-driven technologies. The current classification as a Tangled Rope suggests it still serves a coordination function (clarifying responsibility) but does so with significant, asymmetric extraction. Mandatrophy is not fully resolved as the 'founding problem' is evolving faster than the 'solution' can adapt, leading to calls for new frameworks like shared liability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_complexity,
    'How effectively can primary liability be attributed to developers in complex, opaque, or emergent AI systems where harms arise from interaction effects or unforeseen deployments?',
    'Empirical analysis of legal cases involving AI harms: if courts consistently struggle to identify a single developer as the primary cause, the premise of direct attribution weakens.',
    'If attribution becomes impractical, the constraint''s effective extractiveness on developers may decrease (due to enforcement difficulty), or it may become more theatrical. This could push towards shared_liability or deployer_liability models.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_attribution_complexity, empirical, 'The practical challenge of attributing primary liability to developers in complex technological systems.').

omega_variable(
    innovation_stifling_effect,
    'Does primary developer liability significantly stifle innovation, particularly for smaller entities and open-source projects, by increasing risk and compliance costs?',
    'Economic studies comparing innovation rates and developer participation in jurisdictions with differing liability regimes, or surveys of developer behavior under current frameworks.',
    'If a significant chilling effect is demonstrated, the constraint''s overall societal benefit (coordination) might be outweighed by its negative externalities, strengthening arguments for alternative liability models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_stifling_effect, empirical, 'The impact of developer liability on technological innovation and market entry.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal barriers, financial costs) or internalized (developers self-censoring due to fear of liability)?',
    'Post-regulatory reform analysis: if developers'' risk-averse behavior persists after structural barriers are reduced, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — developers carry the suppression with them, even if legal frameworks soften.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for developers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__developer_liability, theater_ratio, 5, 0.12).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__developer_liability, theater_ratio, 10, 0.15).
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
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, ai_safety_regulation).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, open_source_licensing_norms).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'liability_attribution' kernel, focusing on developer responsibility. It is structurally distinct from 'deployer_liability' and 'shared_liability', which represent alternative allocations of responsibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
