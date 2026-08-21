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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Primary Liability for AI Deployers
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'deployer_liability' reading of
 *   the broader 'liability_attribution' kernel in AI governance. It posits
 *   that the party with direct control over the deployment context and
 *   decision authority should bear primary legal responsibility for
 *   AI-induced harms. This reading aims to ensure accountability and
 *   incentivize safe deployment practices, but it also shifts significant
 *   burdens and risks onto deployers, while shielding developers and
 *   foundation model providers from downstream harm. The metrics reflect the
 *   extractive nature of this shift for deployers.
 *
 * KEY AGENTS:
 *   - Deployers: Primary target (organized/constrained) — bear liability and compliance costs.
 *   - Public Users: Primary beneficiary (organized/constrained) — benefit from increased safety and accountability.
 *   - Developers & Foundation Model Providers: Primary beneficiaries (powerful/arbitrage) — externalize deployment risk.
 *   - Regulators & Courts: Agenda setters (institutional/analytical) — define and enforce the liability framework.
 *   - Small Deployers: Excluded (powerless/trapped) — face prohibitive barriers to entry due to liability burden.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.7).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.6).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.7).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Primary Liability for AI Deployers").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, 'c7d15b19-bb40-42bd-a852-e671462d67af').
narrative_ontology:cs_kernel_codification('c7d15b19-bb40-42bd-a852-e671462d67af', formalized).
narrative_ontology:cs_authority_grounding('c7d15b19-bb40-42bd-a852-e671462d67af', lineage).
narrative_ontology:cs_interpretation_layer_present('c7d15b19-bb40-42bd-a852-e671462d67af').
narrative_ontology:cs_reading_relation('c7d15b19-bb40-42bd-a852-e671462d67af', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('c7d15b19-bb40-42bd-a852-e671462d67af', liability_attribution__shared_liability, coexists_with).
narrative_ontology:cs_axiom('c7d15b19-bb40-42bd-a852-e671462d67af', foundational, control_implies_responsibility).
narrative_ontology:cs_axiom_status(control_implies_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('c7d15b19-bb40-42bd-a852-e671462d67af', control_implies_responsibility, deontological).
narrative_ontology:cs_axiom('c7d15b19-bb40-42bd-a852-e671462d67af', foundational, proximity_to_harm_determines_liability).
narrative_ontology:cs_axiom_status(proximity_to_harm_determines_liability, holdable).
narrative_ontology:cs_axiom_grounding('c7d15b19-bb40-42bd-a852-e671462d67af', proximity_to_harm_determines_liability, conventional).
narrative_ontology:cs_reference_frame('c7d15b19-bb40-42bd-a852-e671462d67af', traditional_product_liability_framework).
narrative_ontology:cs_drift_state('c7d15b19-bb40-42bd-a852-e671462d67af', contemporary_ai_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c7d15b19-bb40-42bd-a852-e671462d67af', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, public_users).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, developers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, small_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations or individuals who integrate and use AI systems in real-world contexts. They bear the primary legal and financial burden for harms caused by deployed AI, requiring extensive due diligence, risk assessment, and potential insurance. Their ability to exit is constrained by the strategic necessity of AI adoption.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, deployers, payer,
    organized, biographical, constrained, global).

% The general public and end-users of AI systems. They benefit from clearer accountability for AI harms, theoretically leading to safer and more trustworthy AI deployments. Their exit options are constrained by the increasing pervasiveness of AI in daily life.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, public_users, beneficiary,
    organized, generational, constrained, global).

% Entities that design, build, and train AI models and systems. Under this liability framework, they externalize much of the deployment-specific risk, allowing them to focus on technical development with reduced direct legal exposure for downstream harms. They can arbitrage regulatory differences.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Large organizations providing general-purpose AI models that are then adapted and deployed by others. This framework shields them from primary liability for specific downstream applications, shifting the burden to the deployers who integrate their models. They have significant market power and global reach.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Government agencies and judicial systems responsible for establishing, interpreting, and enforcing AI liability laws. They define the scope of deployer responsibility and adjudicate cases of AI-induced harm, aiming to create a predictable legal environment.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, regulators_courts, agenda_setter,
    institutional, generational, analytical, national).

% Small businesses or individual practitioners who wish to use AI but lack the resources, expertise, or legal teams to conduct extensive due diligence or manage the high liability risks. They are effectively excluded from deploying advanced AI due to the prohibitive compliance costs.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, small_deployers, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, developers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear point of accountability for AI-related harms, incentivizing careful deployment practices and fostering public trust in AI systems by ensuring redress mechanisms.
% TRANSFER_FUNCTION: Shifts the primary legal and financial burden for AI-induced harms, along with the associated costs of due diligence and risk management, from AI developers and foundation model providers to the entities deploying the AI systems.
% ABSENT_VOICES: Small deployers and AI ethics advocates who argue for a more distributed or developer-centric liability model are often marginalized. Small deployers would highlight the disproportionate burden and barrier to entry, while some ethicists would point to the developers' unique insight into model capabilities and limitations.
% DISAPPEARANCE_RATIONALE: If primary deployer liability vanished, the landscape of AI deployment would fundamentally shift. Developers would face increased pressure to ensure safety, potentially slowing innovation or increasing costs. Public trust might erode without clear accountability, and the adoption of AI could become more chaotic or risk-averse.
% FOUNDING_PROBLEM: The rapid proliferation of AI systems created a 'liability gap' where it was unclear who was responsible for harms, leading to a lack of accountability and potential for unchecked deployment of risky AI.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies, consumer protection agencies, and legal scholars (outside of direct AI industry beneficiaries) corroborate that the problem of AI accountability is still live and evolving, necessitating clear liability frameworks to manage emerging risks.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) for deployers due to the significant legal, financial, and operational costs associated with primary liability. Suppression is moderate (0.6) as deployers are legally bound but can organize to lobby for changes or seek insurance. Theater ratio is low (0.2) because this is a serious legal framework with real consequences, not primarily performative. Accessibility collapse is moderate (0.5) as deployers can choose not to deploy AI, but at the cost of competitive disadvantage. Resistance is moderate (0.5) as deployer groups actively push back against the extent of this liability.
 *
 * PERSPECTIVAL GAP:
 *   Deployers experience this as a highly extractive constraint, imposing substantial costs and risks. Developers and foundation model providers, however, perceive it as a beneficial coordination mechanism that clarifies their role and reduces their direct exposure to downstream harms. The public views it as a necessary safeguard for AI safety. The engine's per-seat classification will highlight these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Deployers are clear targets (high d) as they bear the direct costs and risks. Public users are beneficiaries (low d) due to enhanced safety. Developers and foundation model providers are also beneficiaries (low d) as they externalize significant risk. Regulators are agenda setters, shaping the constraint's operation. Small deployers are excluded, facing effective suppression from the market due to the high barrier to entry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the problem of AI liability is still live and evolving. However, the specific attribution of primary liability to deployers could become a 'false summit' if it fails to genuinely improve AI safety while stifling innovation, or if the actual sources of harm lie more with opaque development practices. The omegas address these potential misattributions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_vs_burden,
    'Is primary deployer liability genuinely effective in ensuring AI safety and accountability, or does it primarily shift the burden without addressing root causes in development or stifling beneficial innovation?',
    'Empirical studies comparing AI safety outcomes and innovation rates in jurisdictions with different liability frameworks, particularly those with more developer-centric or shared liability models.',
    'If found ineffective or overly burdensome, the constraint''s extractiveness for deployers would be re-evaluated as unjustified, potentially leading to a reclassification towards a Snare or a call for a more balanced Tangled Rope. If highly effective, the coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_vs_burden, empirical, 'Whether the liability attribution achieves its stated safety goals or merely reallocates costs.').

omega_variable(
    control_opacity_dilemma,
    'To what extent does the ''control and decision authority'' criterion for deployer liability remain meaningful as AI systems become more autonomous, opaque, and complex, making it difficult for deployers to fully understand or control their behavior?',
    'Legal and technical analysis of specific AI systems, assessing the practical limits of deployer control and the feasibility of due diligence for increasingly black-box models. This would involve expert testimony and case law development.',
    'If deployer control is found to be significantly limited by AI opacity, the justification for primary deployer liability weakens. This could lead to a re-evaluation of the constraint''s ''naturalness'' and a shift towards shared or developer liability, impacting the victim set and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_opacity_dilemma, conceptual, 'The challenge of attributing control and responsibility in complex, opaque AI systems.').

omega_variable(
    small_deployer_exclusion_impact,
    'What is the actual impact of this liability framework on the ability of small and medium-sized enterprises (SMEs) to adopt and innovate with AI, given their limited resources for compliance and risk management?',
    'Quantitative economic studies and qualitative surveys of SMEs in jurisdictions with deployer-centric liability, measuring AI adoption rates, investment in AI, and perceived barriers to entry compared to larger enterprises.',
    'If the framework is found to disproportionately exclude SMEs, the ''excluded'' stakeholder group''s situation would be amplified, and the constraint''s overall suppression and extractiveness would be seen as having a more severe, anti-competitive effect, potentially leading to calls for regulatory carve-outs or support mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_deployer_exclusion_impact, empirical, 'The anti-competitive effect of high liability burdens on small AI deployers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__deployer_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__deployer_liability, theater_ratio, 6, 0.17).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__deployer_liability, theater_ratio, 12, 0.18).
narrative_ontology:measurement(liab_tr_t18, liability_attribution__deployer_liability, theater_ratio, 18, 0.19).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__deployer_liability, theater_ratio, 24, 0.2).
narrative_ontology:measurement(liab_tr_t30, liability_attribution__deployer_liability, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__deployer_liability, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(liab_be_t6, liability_attribution__deployer_liability, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(liab_be_t12, liability_attribution__deployer_liability, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(liab_be_t18, liability_attribution__deployer_liability, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(liab_be_t24, liability_attribution__deployer_liability, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(liab_be_t30, liability_attribution__deployer_liability, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__deployer_liability, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(liab_su_t6, liability_attribution__deployer_liability, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(liab_su_t12, liability_attribution__deployer_liability, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(liab_su_t18, liability_attribution__deployer_liability, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(liab_su_t24, liability_attribution__deployer_liability, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(liab_su_t30, liability_attribution__deployer_liability, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, ai_safety_standards).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, ai_innovation_incentives).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'liability_attribution' kernel. Its structural properties and stakeholder impacts differ significantly from sibling readings focusing on developer or shared liability, necessitating separate constraint stories linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
