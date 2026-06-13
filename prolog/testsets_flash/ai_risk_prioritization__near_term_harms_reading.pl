% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__near_term_harms_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: AI Risk Prioritization: Near-Term Harms Focus
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'near-term harms' reading of AI risk
 *   prioritization, asserting that deployed AI systems currently cause
 *   measurable discrimination, displacement, and surveillance, and therefore
 *   justice-oriented interventions are paramount. This framing directs
 *   resources and attention towards bias audits, worker protections, and
 *   surveillance regulation, often at the expense of long-term, speculative
 *   AI safety concerns. It is actively enforced through funding priorities,
 *   academic discourse, and policy advocacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.7).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "AI Risk Prioritization: Near-Term Harms Focus").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '188256af-4fbb-4ff9-9bbe-e209d178f2c5').
narrative_ontology:cs_kernel_codification('188256af-4fbb-4ff9-9bbe-e209d178f2c5', distributed).
narrative_ontology:cs_authority_grounding('188256af-4fbb-4ff9-9bbe-e209d178f2c5', practice).
narrative_ontology:cs_interpretation_layer_present('188256af-4fbb-4ff9-9bbe-e209d178f2c5').
narrative_ontology:cs_reading_relation('188256af-4fbb-4ff9-9bbe-e209d178f2c5', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('188256af-4fbb-4ff9-9bbe-e209d178f2c5', foundational, present_suffering_demands_priority).
narrative_ontology:cs_axiom_status(present_suffering_demands_priority, holdable).
narrative_ontology:cs_axiom_grounding('188256af-4fbb-4ff9-9bbe-e209d178f2c5', present_suffering_demands_priority, deontological).
narrative_ontology:cs_axiom('188256af-4fbb-4ff9-9bbe-e209d178f2c5', secondary, speculative_risk_is_distraction).
narrative_ontology:cs_axiom_status(speculative_risk_is_distraction, holdable).
narrative_ontology:cs_axiom_grounding('188256af-4fbb-4ff9-9bbe-e209d178f2c5', speculative_risk_is_distraction, empirically_contingent).
narrative_ontology:cs_reference_frame('188256af-4fbb-4ff9-9bbe-e209d178f2c5', justice_first_ai_governance).
narrative_ontology:cs_drift_state('188256af-4fbb-4ff9-9bbe-e209d178f2c5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('188256af-4fbb-4ff9-9bbe-e209d178f2c5', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, social_justice_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, ai_developers_deployers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, long_term_ai_safety_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from interventions addressing algorithmic bias, surveillance, and job displacement. Their lived experiences are centered in the risk discourse, leading to policy and research efforts aimed at their protection and empowerment. However, their ability to fully exit the impacts of AI systems remains constrained.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    organized, generational, constrained, global).

% Receive increased funding, academic recognition, and policy influence for their work on identifying and mitigating near-term AI harms. Their research agenda is prioritized, and they are seen as essential experts in the field. They have mobility within academia and policy circles.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    powerful, biographical, mobile, global).

% Their advocacy for justice-oriented AI governance is amplified and integrated into policy discussions. They gain leverage in shaping regulatory frameworks and corporate practices. Their exit options are constrained by the systemic nature of the issues they address.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, social_justice_advocates, beneficiary,
    organized, generational, constrained, global).

% Bear the costs of increased regulation, compliance, bias audits, and potential legal liabilities related to near-term harms. Their development cycles and deployment strategies are influenced by these priorities, potentially slowing innovation or increasing operational costs. Exiting the AI development space is difficult due to sunk costs and market position.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_developers_deployers, payer,
    institutional, biographical, constrained, global).

% Experience a redirection of funding, academic attention, and public discourse away from their focus on existential or catastrophic AI risks. Their work is often framed as speculative or less urgent, making it harder to secure grants, publish, or attract talent. Their exit options are constrained by their specialized expertise and career path dependence.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, long_term_ai_safety_researchers, payer,
    moderate, civilizational, constrained, global).

% Are responsible for translating risk assessments into actionable policy. They navigate competing priorities and allocate resources based on prevailing risk framings. This constraint empowers them to enact regulations focused on present-day harms, aligning with public demand for immediate solutions. They have mobility across policy areas.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse stakeholders (activists, researchers, policymakers) to identify, measure, and mitigate tangible, present-day harms caused by deployed AI systems, ensuring that ethical considerations are integrated into development and deployment.
% TRANSFER_FUNCTION: Transfers resources (funding, academic prestige, policy influence) from speculative, long-term AI risk research and unrestricted AI development towards research, advocacy, and regulatory efforts focused on immediate social justice and equity concerns in AI.
% ABSENT_VOICES: Proponents of an exclusive focus on existential AI risk are often marginalized in this discourse, their concerns framed as distractions from urgent present-day injustices. They would argue for a re-prioritization of resources towards foundational alignment research.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, the focus on immediate AI harms would dissipate, leading to a significant reduction in funding for bias audits, fairness research, and regulatory efforts. AI development would likely proceed with less scrutiny regarding social impacts, and marginalized communities would lose a key advocacy framework, leading to a reorganization of the AI ethics and governance landscape.
% FOUNDING_PROBLEM: The initial problem was the rapid deployment of AI systems without adequate consideration for their social impacts, leading to documented cases of algorithmic discrimination, privacy violations, and job displacement, disproportionately affecting vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Marginalized communities, social justice organizations, and independent investigative journalists consistently attest that the problems of algorithmic bias, surveillance, and displacement are not only live but intensifying. Their reports and lived experiences provide corroboration from outside the direct beneficiaries of this prioritization.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates efforts to address real, present harms (beneficiaries: marginalized communities, fairness researchers) while simultaneously extracting resources and attention from alternative risk framings (victims: AI developers, long-term AI safety researchers). Extractiveness (0.65) is moderate-high due to the redirection of significant funding and career opportunities. Suppression (0.7) is high, as alternative framings (e.g., existential risk) are often dismissed as speculative or distracting. Theater ratio (0.2) is low, indicating that the focus on near-term harms is largely genuine, though some performative aspects may exist in policy debates. Resistance (0.8) is high from those whose work is de-prioritized or whose systems are targeted for regulation.
 *
 * PERSPECTIVAL GAP:
 *   Marginalized communities and social justice advocates experience this as a necessary coordination mechanism to address urgent injustices. AI developers and long-term AI safety researchers, however, experience it as an extractive and suppressive force that diverts resources from what they perceive as more critical or foundational problems. The engine will compute these divergent classifications based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and fairness/accountability researchers are primary beneficiaries (d near 0.0) as the constraint directly addresses their concerns and provides funding/career opportunities. AI developers/deployers are victims (d near 1.0) as they face increased regulation and scrutiny. Long-term AI safety researchers are also victims (d near 1.0) due to the redirection of funding and academic prestige. Social justice advocates are beneficiaries (d near 0.0) as their agenda is prioritized. Policy makers are agenda-setters, balancing competing priorities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate is actively contested and evolving. The classification as Tangled Rope prevents mislabeling it as a pure Snare (ignoring the genuine coordination function for addressing present harms) or a pure Rope (ignoring the asymmetric extraction from other risk framings). The ongoing debate about its founding problem status ('contested') reflects this active, rather than atrophied, state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine prioritization of urgent harms, or a rhetorical framing to redirect resources from other risk areas?',
    'Analysis of resource allocation patterns over time: if resources consistently flow to near-term interventions without evidence of genuine harm reduction, it suggests a rhetorical redirection.',
    'If a rhetorical redirection, the constraint''s extractiveness from other risk areas (e.g., long-term safety research) is higher, and its coordination function is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''near_term_harms_reading'' of the ''ai_risk_prioritization'' kernel. A sibling reading, ''existential_risk_reading'', would prioritize long-term, extinction-level threats. The disagreement is located in the primary victim set and resource allocation priorities.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of long-term risk discourse structural (e.g., funding mechanisms) or internalized (e.g., social pressure within academic fields)?',
    'Post-intervention discourse trajectory: if long-term risk concerns persist or re-emerge after near-term interventions are implemented, it suggests structural suppression; if they remain marginalized, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism regarding alternative AI risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_prioritization' kernel, focusing on near-term harms. It is structurally distinct from the 'existential_risk_reading' which prioritizes long-term threats, as their victim sets, resource allocations, and timescales differ significantly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
