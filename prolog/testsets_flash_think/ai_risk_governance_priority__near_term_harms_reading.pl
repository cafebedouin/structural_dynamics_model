% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance Prioritization: Near-Term Harms
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents a normative claim: that AI risk governance
 *   *must prioritize* mitigating demonstrated present harms (bias,
 *   misinformation, labor displacement, surveillance) affecting marginalized
 *   populations now. It is a reading of the broader 'AI risk governance
 *   priority' kernel. While the *advocated* constraint itself is intended as
 *   a 'rope' (a coordination mechanism for effective governance), the
 *   authored metrics reflect the *current standing arrangement* where these
 *   harms are *not* prioritized. This current arrangement is highly
 *   extractive from marginalized groups, and its persistence is enabled by
 *   the diversion of attention and resources to other risk framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.85).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.75).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance Prioritization: Near-Term Harms").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, 'b903cdff-8486-4e2e-abaa-b3f2b4adb1c1').
narrative_ontology:cs_kernel_codification('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', distributed).
narrative_ontology:cs_authority_grounding('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', extraction).
narrative_ontology:cs_interpretation_layer_present('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1').
narrative_ontology:cs_reading_relation('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', ai_risk_governance_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', foundational, demonstrated_harm_requires_immediate_action).
narrative_ontology:cs_axiom_status(demonstrated_harm_requires_immediate_action, holdable).
narrative_ontology:cs_axiom_grounding('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', demonstrated_harm_requires_immediate_action, deontological).
narrative_ontology:cs_axiom('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', secondary, speculative_risk_should_not_preempt_present_suffering).
narrative_ontology:cs_axiom_status(speculative_risk_should_not_preempt_present_suffering, holdable).
narrative_ontology:cs_axiom_grounding('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', speculative_risk_should_not_preempt_present_suffering, deontological).
narrative_ontology:cs_reference_frame('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', unregulated_innovation_paradigm).
narrative_ontology:cs_drift_state('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', contemporary_ai_deployment_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b903cdff-8486-4e2e-abaa-b3f2b4adb1c1', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_developers_and_deployers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, investors).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, displaced_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities benefit from the current lack of prioritization of near-term harms, allowing them to deploy AI systems rapidly without incurring significant costs for bias mitigation, labor retraining, or robust oversight. They currently set the de facto agenda for AI development.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_developers_and_deployers, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, ai_developers_and_deployers, agenda_setter).

% Investors profit from the rapid, unconstrained deployment of AI technologies, where the costs of present harms are externalized. Prioritizing these harms would introduce regulatory friction and potential financial liabilities, reducing short-term returns.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, investors, beneficiary,
    powerful, biographical, arbitrage, global).

% These populations disproportionately bear the costs of AI-driven surveillance, resource extraction, and algorithmic bias, often with limited recourse or ability to influence the development and deployment of these systems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, generational, trapped, global).

% Communities facing algorithmic discrimination in areas like housing, employment, and criminal justice, as well as those targeted by misinformation campaigns. They are often structurally trapped within systems that perpetuate these harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_communities, payer,
    powerless, generational, trapped, national).

% Workers whose livelihoods are disrupted or eliminated by automation without adequate retraining, social safety nets, or alternative employment opportunities. Their options are constrained by economic realities and policy choices.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, displaced_workers, payer,
    powerless, biographical, constrained, national).

% These organizations actively advocate for the prioritization of near-term harms, conducting research, raising awareness, and lobbying for policy changes. They seek to shift the governance agenda towards accountability and mitigation.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, civil_society_organizations, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, civil_society_organizations, observer).

% These actors have the power to implement and enforce policies that prioritize near-term harms, but often face lobbying pressure from industry and competing priorities. Their ability to act is constrained by political will and institutional capacity.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, governments_and_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, governments_and_regulators, observer).

% Advocates who prioritize speculative existential risks from advanced AI. From the perspective of this 'near-term harms' reading, their focus diverts attention and resources from immediate, demonstrable harms, effectively excluding their concerns from this specific prioritization framework.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, x_risk_advocates, excluded,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, ai_developers_and_deployers).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global and national efforts, resources, and regulatory frameworks towards identifying, measuring, and mitigating the immediate, tangible negative impacts of AI systems on human populations and social structures.
% TRANSFER_FUNCTION: This constraint seeks to transfer attention, funding, and regulatory pressure from speculative, long-term AI risks to immediate, demonstrated harms. It would shift the burden of harm from affected communities to AI developers and deployers, who would bear the costs of mitigation and responsible development.
% ABSENT_VOICES: Future AI systems (as potential agents), and speculative existential risk scenarios (as a primary framing) are largely absent from this prioritization, which focuses on present, human-centric harms. Their concerns are not directly addressed within this specific framework.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, the current trajectory of AI development would continue largely unchecked regarding present harms. Resources and regulatory attention would likely flow back towards more speculative, long-term risks, and the demonstrable negative impacts on marginalized populations would continue to accumulate and intensify, reorganizing the social and economic landscape around unmitigated AI harms.
% FOUNDING_PROBLEM: The rapid, often unchecked, deployment of AI systems without adequate consideration or mitigation of their immediate, demonstrable negative impacts on vulnerable populations, leading to widespread algorithmic bias, misinformation, labor displacement, and surveillance.
% FOUNDING_PROBLEM_CORROBORATION: The problem of unmitigated present harms is ongoing and intensifying, corroborated by extensive reports from civil society organizations (e.g., AI Now Institute, Access Now), academic studies on algorithmic bias, investigative journalism, and direct testimony from affected communities globally. This evidence comes from sources outside the direct beneficiaries of the current AI development paradigm.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the severe and accumulating harms borne by marginalized populations due to unmitigated AI deployment. Suppression (0.75) is high because these groups often lack the power and institutional channels to effectively resist or exit systems causing harm. The theater ratio (0.45) indicates that while there is growing discourse and some performative action around AI ethics, a significant portion of activity does not translate into structural mitigation of present harms. Resistance (0.80) is strong from civil society and affected communities, but often struggles against entrenched power. Accessibility collapse (0.65) reflects the limited alternatives available to those impacted by pervasive AI systems.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of AI developers and investors, the current state (lack of prioritization of present harms) might be viewed as efficient innovation, with any 'harms' being acceptable externalities or solvable later. From the perspective of affected communities, the same situation is one of ongoing, severe extraction and injustice. The 'claimed_type' of 'rope' reflects the ideal coordination mechanism that advocates for this prioritization envision, while the high 'extractiveness' reflects the reality of the status quo it seeks to change.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers, deployers, and investors are structural beneficiaries of the *lack* of this prioritization, as they profit from rapid deployment without bearing the full costs of present harms. Global South populations, marginalized communities, and displaced workers are the primary targets, bearing the brunt of these harms. Civil society organizations and some governments act as agenda-setters, advocating for this prioritization. X-risk advocates are structurally excluded from this specific framing, as their focus lies elsewhere.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_tradeoff,
    'To what extent would a strong prioritization of near-term harms divert essential resources (funding, talent, regulatory attention) from addressing long-term or existential AI risks?',
    'Empirical analysis of resource flows and policy outcomes in jurisdictions that adopt either near-term or long-term prioritization, or a ''bridge'' approach. Track funding, research focus, and regulatory mandates.',
    'If diversion is severe, this prioritization might inadvertently increase long-term risks. If diversion is minimal or synergistic, it strengthens the case for this reading as a holistic approach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_tradeoff, empirical, 'Trade-off between addressing present harms and long-term/existential risks.').

omega_variable(
    causal_link_present_to_future_harms,
    'Are the mechanisms that produce present AI harms (e.g., bias, lack of accountability) structurally linked to the pathways that could lead to future catastrophic or existential risks, such that mitigating one inherently mitigates the other?',
    'Conceptual and empirical research into the systemic properties of AI development and deployment, identifying shared root causes (e.g., power asymmetries, lack of transparency) that contribute to both near-term and far-term risks.',
    'If a strong causal link exists, this reading''s prioritization gains additional justification as a foundational step for all AI safety. If links are weak, the two risk categories might require distinct governance approaches.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_link_present_to_future_harms, conceptual, 'Structural entanglement of near-term and far-term AI risks.').

omega_variable(
    industry_resistance_to_mitigation_costs,
    'Is the industry''s resistance to prioritizing near-term harms primarily driven by genuine technical difficulty and cost, or by a strategic diversion of regulatory attention to less tractable, speculative risks?',
    'Regulatory discovery and independent audits of AI development costs, alongside analysis of industry lobbying efforts and public messaging. Compare stated technical challenges with actual mitigation capabilities.',
    'If resistance is primarily strategic, it strengthens the ''snare'' classification of the current non-prioritization and highlights the extractive nature of the status quo. If costs are genuinely prohibitive, it points to a different class of coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_resistance_to_mitigation_costs, empirical, 'Motivation behind industry''s resistance to present harm mitigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2015, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(ai_r_tr_t2017, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2017, 0.35).
narrative_ontology:measurement(ai_r_tr_t2019, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2019, 0.39).
narrative_ontology:measurement(ai_r_tr_t2021, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2021, 0.42).
narrative_ontology:measurement(ai_r_tr_t2023, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2023, 0.44).
narrative_ontology:measurement(ai_r_tr_t2025, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2015, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(ai_r_be_t2017, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2017, 0.74).
narrative_ontology:measurement(ai_r_be_t2019, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2019, 0.78).
narrative_ontology:measurement(ai_r_be_t2021, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2021, 0.81).
narrative_ontology:measurement(ai_r_be_t2023, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2023, 0.83).
narrative_ontology:measurement(ai_r_be_t2025, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2015, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(ai_r_su_t2017, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2017, 0.64).
narrative_ontology:measurement(ai_r_su_t2019, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(ai_r_su_t2021, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2021, 0.71).
narrative_ontology:measurement(ai_r_su_t2023, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2023, 0.73).
narrative_ontology:measurement(ai_r_su_t2025, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, algorithmic_accountability_laws).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_development_funding_priorities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
